<?php

namespace App\Controllers;

use App\Models\ProjectModel;
use App\Models\ResultsModel;
use App\Models\UserModel;
use App\Models\VariablesModel;
use PhpOffice\PhpSpreadsheet\Spreadsheet;
use PhpOffice\PhpSpreadsheet\Worksheet\Worksheet;
use PhpOffice\PhpSpreadsheet\Writer\Xlsx;
use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\UploadedFileInterface;
use Slim\Logger;
use Slim\Psr7\Stream;
use Symfony\Component\Finder\Exception\AccessDeniedException;

class VariablesController extends Controller
{


	public function action_restoreDefaultVariablesAPI() {
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id'], false);

		if ((in_array($user->app_mode,['global','countries']) && $user->role !== UserModel::ROLE_SYSADMIN) && $project['user_id'] !== $user->id) {
			$this->deny();
		}

		$varsModel = new VariablesModel($this->db);
		try {
			$varsModel->resetVariables($project);
		} catch (\Exception $e) {
			return $this->respondWithData(['status' => 'error']);
		}
		return $this->respondWithData(['status' => 'success']);
	}

	/**
	 * @return Response
	 * @throws \Slim\Exception\HttpUnauthorizedException
	 */
	public function action_getVariablesByProjectIdAPI()
	{
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id'], true);

		if ((in_array($user->app_mode,['global','countries']) && $user->role !== UserModel::ROLE_SYSADMIN) && $project['user_id'] !== $user->id) {
			$this->deny();
		}

		$result = [];
		$varsModel = new VariablesModel($this->db);
		$vars = $varsModel->fetchVariables($project);
		foreach ($vars AS $var) {
			//Include only PROJECT variables
			if ($var['type_of_variable'] !== 'PROJECT') continue;
			$result[] = [
				'is_custom' => isset($var['is_custom']) ? true : false,
				'description' => $var['description'],
				'value' => $var['value'],
				'unit' => $var['unit'],
				'var_class' => $var['var_class'],
				'technology' => empty($var['technology']) ? 'OTHER' : $var['technology']
			];
		}
		return $this->respondWithData($result);
	}

	/**
	 * @return Response
	 * @throws \PhpOffice\PhpSpreadsheet\Writer\Exception
	 * @throws \Slim\Exception\HttpUnauthorizedException
	 */
	public function action_downloadVariablesByProjectIdAPI() {
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id'], true);

		if ((in_array($user->app_mode,['global','countries']) && $user->role !== UserModel::ROLE_SYSADMIN) && $project['user_id'] !== $user->id) {
			$this->deny();
		}

		$varsModel = new VariablesModel($this->db);
		$vars = $varsModel->fetchVariables($project);

		//Group vars
		$gv = [];
		foreach ($vars AS $var) {
			if ($var['type_of_variable'] !== 'PROJECT') continue;
			$var['technology'] = empty($var['technology']) ? 'OTHER' : $var['technology'];
			if (empty($gv[$var['var_class']])) {
				$gv[$var['var_class']] = [];
			}
			if (empty($gv[$var['var_class']][$var['technology']])) {
				$gv[$var['var_class']][$var['technology']] = [];
			}
			$gv[$var['var_class']][$var['technology']][] = $var;
		}

		//Process data and fill out the template
		$spreadsheet = new \PhpOffice\PhpSpreadsheet\Spreadsheet();
		$sheet = $spreadsheet->getActiveSheet();

		$row = 1;
		foreach ($gv AS $group=>$techs) {
			//$sheet->setCellValueByColumnAndRow(1,$row,$group);
			$sheet->getCellByColumnAndRow(1,$row)->setValue($group)->getStyle()->getFont()->setBold(true);
			$row++;
			foreach ($techs AS $tech=>$vars) {
				$sheet->setCellValueByColumnAndRow(1,$row,$tech);
				$row++;
				foreach ($vars as $var) {
					$sheet->setCellValueByColumnAndRow(1, $row, $var['value']);
					$sheet->setCellValueByColumnAndRow(2, $row, $var['unit']);
					$sheet->setCellValueByColumnAndRow(3, $row, $var['description']);
					$sheet->setCellValueByColumnAndRow(4, $row, $var['id']);
					$row++;
				}
			}
		}

		$sheet->getStyle('A1')->getNumberFormat()->setFormatCode(\PhpOffice\PhpSpreadsheet\Style\NumberFormat::FORMAT_NUMBER_00);
		$sheet->getColumnDimension('A')->setAutoSize(true);
		$sheet->getColumnDimension('B')->setAutoSize(true);
		$sheet->getColumnDimension('C')->setAutoSize(true);
		$sheet->getColumnDimension('D')->setVisible(false);

		//Save the results to a temporary file
		$saved_filename = tempnam('/tmp', 'vars' . $this->args['id']);
		setlocale(LC_ALL, 'en_US');
		$writer = new Xlsx($spreadsheet);
		$writer->save($saved_filename);


		//Server template to end user
		$fh = fopen($saved_filename, 'r');
		fseek($fh, 0);
		$stream = new Stream($fh);
		$filename = urldecode($project['name']) . ' variables template';
		$filename = ResultsModel::stripFilenameCharacters($filename).'.xlsx';

		return $this->response->withHeader('Content-Type', 'application/force-download')
			->withHeader('Content-Type', 'application/octet-stream')
			->withHeader('Content-Type', 'application/download')
			->withHeader('Content-Description', 'File Transfer')
			->withHeader('Content-Transfer-Encoding', 'binary')
			->withHeader('Content-Disposition', 'attachment; filename="' . $filename . '"')
			->withHeader('Expires', '0')
			->withHeader('Cache-Control', 'must-revalidate, post-check=0, pre-check=0')
			->withHeader('Pragma', 'public')
			->withBody($stream);
	}

	/**
	 * @throws \Slim\Exception\HttpUnauthorizedException
	 */
	public function action_addVariablesFromXLSXToProjectAPI() {
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id'], true);

		if ((in_array($user->app_mode,['global','countries']) && $user->role !== UserModel::ROLE_SYSADMIN) && $project['user_id'] !== $user->id) {
			$this->deny();
		}

		/**
		 * @var UploadedFileInterface
		 **/
		$files = $this->request->getUploadedFiles();

		if (strpos($_ENV['ENV'],'prod') !== false && !isset($files['file'])) {
			throw new \Exception('File was not uploaded', 500);
		}

		$varsModel = new VariablesModel($this->db);
		$vars = $varsModel->fetchVariables($project);
		$main_vars = [];

		foreach ($varsModel->fetchMainVariables() AS $mv) {
			$main_vars[$mv['id']] = $mv;
		}
		$existing_vars = [];
		foreach ($vars AS $var) {
			//Skip non project
			if ($var['type_of_variable'] !== 'PROJECT') continue;
			$existing_vars[] = [trim($var['id']), (float)trim($var['value'])];
		}

		//Parse file
		$saved_filename = tempnam('/tmp', 'uploadedvars' . $this->args['id']);
		if (strpos($_ENV['ENV'],'prod') !== false) {
			/**
			 * $files \Slim\Psr7\UploadedFile
			 */
			$files['file']->moveTo($saved_filename);
		} else {
			$saved_filename = "/home/itsaryuk/Downloads/vars2.xlsx";
		}
		$spreadsheet = \PhpOffice\PhpSpreadsheet\IOFactory::load($saved_filename);
		$sheet = $spreadsheet->getActiveSheet();
		$new_vars = [];
		for ($i=1;$i<500;$i++) {
			$var_id = $sheet->getCellByColumnAndRow(4,$i)->getValue();
			$var_value = $sheet->getCellByColumnAndRow(1,$i)->getValue();
			if (empty($var_id) && $var_value == '') {
				break;//End of vars
			}
			if (!empty($var_id) && $var_value !== '') {
				//Convert and validate variable
				try {
					$new_vars[] = [trim($var_id), $this->variableValidateConvert($var_value, $var_id, $main_vars)];
				} catch (\Exception $e) {
					//Add error to vars validation log
					$this->logger->warning("Var validation error, " . $var_id . ': ' . $var_value);
				}
			}
		}

		$new_vars = $this->validateVars($new_vars, $main_vars);

		$updated_vars = [];
		foreach ($existing_vars AS $ev) {
			foreach ($new_vars AS $nv) {
				if ($ev[0] == $nv[0] && $ev[1] != $nv[1]) {
					//Var changed
					$updated_vars[] = $nv;
				}
			}
		}
		die(var_dump($updated_vars));
		if (sizeof($updated_vars)>0) {
			$varsModel->updateProjectVars($project, $updated_vars);
		}

		return $this->respondWithData('uploaded');
	}

	private function variableValidateConvert($var_value, $var_id, $main_vars) {
		$var_value = trim($var_value);
		//Cast value
		switch ($main_vars[$var_id]['type_of_value']) {
			case 'INT':
				$var_value = (int)$var_value;
				break;
			case 'DOUBLE':
				$var_value = (float)$var_value;
				break;
		}
		//Min-max validation
		if ($var_value < $main_vars[$var_id]['min_val'] || $var_value > $main_vars[$var_id]['max_val']) {
			throw new \Exception('Range validation failed');
		}
		return $var_value;
	}

	private function validateVars($vars, $main_vars) {
		$final_vars = [];
		$new_values_by_name = [];
		$exclude_vars = [];//ID of vars to exclude
		//$vars - [id, value]
		//$main_vars - array of var records from db, index - var id
		foreach ($vars AS $var) {
			$new_values_by_name[$main_vars[$var[0]]['name']] = [$var[0], $var[1]];
		}

		//GROUPS VALIDATIONS CODE HERE
        $var_arr1 = $new_values_by_name['InitialDataFOCL.CableDuctCoeff'];
		$CableDuctCoeff = $var_arr1[1];

        $var_arr2 = $new_values_by_name['InitialDataFOCL.CableLayingMachineCoeff'];
        $CableLayingMachineCoeff = $var_arr2[1];

        if (($CableDuctCoeff + $CableLayingMachineCoeff) != 1)
        {
            $id1 = $var_arr1[0];
            $id2 = $var_arr2[0];
            $exclude_vars[] = $id1;
            $exclude_vars[] = $id2;
        }

		//Filter out invalid vars
		foreach ($vars AS $var) {
			if (!in_array($var[0], $exclude_vars)){
				$final_vars[] = $var;
			}
		}

		return $final_vars;
	}

}
