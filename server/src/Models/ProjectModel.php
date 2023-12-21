<?php

namespace App\Models;

use PDO;

class ProjectModel {

	/**
	 * @var PDO
	 */
	protected $db;

	/**
	 * Where to save data
	 * @var string ['results','reports']
	 */
	protected $dirs;

	/**
	 * ProjectModel constructor.
	 * @param $db
	 * @param $dirs
	 */
	public function __construct($db, $dirs = []) {
		$this->db = $db;
		$this->dirs = $dirs;
	}

	public function generateReport($project_id, $input_data, $type) {
		$resultsModel = new ResultsModel($this->db);
		$objects = $resultsModel->getResultsByProjectId('object', $project_id);
		$topology = $resultsModel->getResultsByProjectId('topology', $project_id);
		if (!empty($topology)) {
			$topology = $topology[0];
		}
		//Prepare output template
		$refl = new \ReflectionClass('App\Models\OutputTemplates\\' . $input_data->output_template->class);
		$output_template_file_name = __DIR__ . '/../Models/OutputTemplates/' . $input_data->output_template->filename;
		$export = $refl->newInstanceArgs([$output_template_file_name]);

		//Process data and fill out the template
		switch ($type) {
			case 'simple':
				$export->process($input_data->objects, $input_data->calculation_parameters, ['objects' => $objects, 'topology' => $topology]);
				break;
			case 'smart':
				$export->process_smart($input_data->objects, $input_data->calculation_parameters, ['objects' => $objects, 'topology' => $topology]);
				break;
		}

		$dir = '/tmp';
		$filename = tempnam($dir, $type);
		chmod($filename,0766);
		//Save the results to a temporary file
		$export->save($filename);
		return $filename;
	}

	public function generateSmartReport($project_id, $input_data) {
		return $this->generateReport($project_id, $input_data, 'smart');
	}

	public function generateSimpleReport($project_id, $input_data) {
		return $this->generateReport($project_id, $input_data, 'simple');
	}

	public function saveResult($run_id, $project_id, $data, $type, $object_id = null, $step = 'main') {
		switch ($type) {
			case 'object':
				$this->saveObjectResult($run_id, $project_id, $data, $object_id, $step);
				break;
			case 'object-log':
				$this->saveObjectLogResult($run_id, $project_id, $data, $object_id, $step);
				break;
			case 'topology':
				$this->saveTopolgyResult($run_id, $project_id, $data, $step);
				break;
			case 'topology-log':
				$this->saveTopolgyLogResult($run_id, $project_id, $data, $step);
				break;
			case 'report-simple': case 'report-smart':
				$this->saveReportResult($run_id, $project_id, $data, $step, $type);
				break;
		}
	}

	private function saveReportResult($run_id, $project_id, $data, $step, $type) {
		$this->saveDBReport($run_id, $project_id, str_replace('report-','',$type), $data,  $step);
	}

	private function saveTopolgyResult($run_id, $project_id, $data, $step) {
		$this->saveDBResult($run_id, $project_id, 'topology', $data, null, $step);
	}

	private function saveTopolgyLogResult($run_id, $project_id, $data, $step) {
		$this->saveDBLog($run_id, $project_id, 'topology', $data, null, $step);
	}

	private function saveObjectLogResult($run_id, $project_id, $data, $object_id, $step) {
		$this->saveDBLog($run_id, $project_id, 'object', $data, $object_id, $step);
	}

	private function saveObjectResult($run_id, $project_id, $data, $object_id, $step) {
		$this->saveDBResult($run_id, $project_id, 'object', $data, $object_id, $step);
	}

	private function saveDBResult($run_id, $project_id, $type, $data, $object_id, $step) {
		try {
			$sql = "INSERT INTO calculation_results SET `run_id`=:run_id, `project_id`=:project_id, `type`=:type, `data`=:data, `step`=:step, `created`=NOW()";
			if ($object_id !== null) {
				$sql .= ", object_id=:object_id";
				$insert = $this->db->prepare($sql);
				$insert->bindParam(':object_id', $object_id);
			} else {
				$insert = $this->db->prepare($sql);
			}
			$insert->bindParam(':project_id', $project_id);
			$insert->bindParam(':type', $type);
			$insert->bindParam(':data', $data, PDO::PARAM_LOB);
			$insert->bindParam(':step', $step);
			$insert->bindParam(':run_id', $run_id

			);
			$insert->execute();
		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}
	}

	private function saveDBLog($run_id, $project_id, $type, $data, $object_id, $step) {
		try {
			$sql = "INSERT INTO calculation_logs SET `run_id`=:run_id, `project_id`=:project_id, `type`=:type, `data`=:data, `step`=:step, `created`=NOW()";
			if ($object_id !== null) {
				$sql .= ", object_id=:object_id";
				$insert = $this->db->prepare($sql);
				$insert->bindParam(':object_id', $object_id);
			} else {
				$insert = $this->db->prepare($sql);
			}
			$data = ResultsModel::compressData($data);
			$insert->bindParam(':project_id', $project_id);
			$insert->bindParam(':type', $type);
			$insert->bindParam(':data', $data, PDO::PARAM_LOB);
			$insert->bindParam(':step', $step);
			$insert->bindParam(':run_id', $run_id);
			$insert->execute();
		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}
	}

	private function saveDBReport($run_id, $project_id, $type, $data, $step) {
		try {
			$sql = "INSERT INTO calculation_reports SET `run_id`=:run_id, `project_id`=:project_id, `type`=:type, `data`=:data, `step`=:step, `created`=NOW()";
			$insert = $this->db->prepare($sql);
			$data = ResultsModel::compressData($data);
			$insert->bindParam(':project_id', $project_id);
			$insert->bindParam(':type', $type);
			$insert->bindParam(':data', $data, PDO::PARAM_LOB);
			$insert->bindParam(':step', $step);
			$insert->bindParam(':run_id', $run_id);
			$insert->execute();
		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}
	}

	public function duplicateProject($project_id) {
		try {

			$insert = $this->db->prepare("INSERT INTO projects
					( name, created, description, identifier, user_id, country_id, settings, mode)
					SELECT  CONCAT(name,' (copy)'), UTC_TIMESTAMP(), description, ?, user_id, country_id, settings, mode  FROM projects WHERE id=?");

			$ident = $this->generateIdentifier();
			$insert->bindParam(1, $ident);
			$insert->bindParam(2, $project_id);

			$insert->execute();

		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $this->db->lastInsertId();
	}

	/**
	 * @param array $project
	 * @return int $project_id
	 */
	public function addProject($project) {
		try {

			$insert = $this->db->prepare("INSERT INTO projects
				( name, created, description, identifier, user_id, country_id, settings, mode ) VALUES ( ?, UTC_TIMESTAMP(), ?, ?, ?, ?, ?, ? )");

			$name = substr($project['name'], 0, 240);
			$name = mb_ereg_replace("([^\w\s\d\-_~,;'\[\]\(\).])", '', $name);
			$name = mb_ereg_replace("([\.]{2,})", '', $name);
			$insert->bindParam(1, $name);
			$insert->bindParam(2, $project['description']);
			$insert->bindParam(3, $project['identifier']);
			$insert->bindParam(4, $project['user_id']);
			$insert->bindParam(5, $project['country_id']);
			$insert->bindParam(6, $project['settings']);
			$insert->bindParam(7, $project['mode']);

			$insert->execute();

		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $this->db->lastInsertId();
	}

	/**
	 * @param string $identifier
	 * @return bool
	 * @throws \Exception
	 */
	public function validateIdentifierAlreadyExist($identifier) {

		try {
			$getData = $this->db->prepare("SELECT id FROM projects WHERE identifier=:identifier");
			$getData->bindParam(':identifier', $identifier, PDO::PARAM_STR);
			$getData->execute();

			$resultData = $getData->fetch(PDO::FETCH_ASSOC);

			if (empty($resultData)) {
				return false;
			}

		} catch (\Exception $e) {
			throw new \Exception($e->getMessage());
		}

		return true;
	}

	/**
	 * @param array $variable
	 * @return int $row_id
	 * @throws \Exception
	 */
	public function addVariableToProject($variable) {

		try {

			$insert = $this->db->prepare("INSERT INTO projects_has_variables
				( projects_id, variables_id, value ) VALUES ( ?, ?, ? )");

			$insert->bindParam(1, $variable['projects_id']);
			$insert->bindParam(2, $variable['variables_id']);
			$insert->bindParam(3, $variable['value']);

			$insert->execute();

		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $this->db->lastInsertId();
	}

	/**
	 * @param array $variable
	 * @return int $row_id
	 * @throws \Exception
	 */
	public function updateVariableInProject($variable) {

		try {

			$insert = $this->db->prepare("UPDATE projects_has_variables SET value=:value WHERE projects_id=:projects_id AND variables_id=:variables_id");

			$insert->bindParam(':projects_id', $variable['projects_id']);
			$insert->bindParam(':variables_id', $variable['variables_id']);
			$insert->bindParam(':value', $variable['value']);

			$insert->execute();

		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $this->db->lastInsertId();
	}

	/**
	 * @param int $projectId
	 * @param bool $fetchall
	 * @return array $project
	 * @throws \Exception
	 */
	public function getProjectById(int $projectId, $fetchall = false) {

		try {

			if ($fetchall) {
				$getData = $this->db->prepare("SELECT * FROM projects WHERE id=:id");
			} else {
				$getData = $this->db->prepare("SELECT id,name,user_id,description,country_id,status,progress,created,updated,
				settings,IF(templatedata IS NOT NULL, 1, 0) AS templatedata_uploaded,templatedata_upload_time,validationdata,statusdata, mode FROM projects WHERE id=:id");
			}

			$getData->bindParam(':id', $projectId);

			$getData->execute();

			$resultData = $getData->fetch(PDO::FETCH_ASSOC);
			//Uncompress
			if (!empty($resultData['validationdata'])) $resultData['validationdata'] = ResultsModel::decompressData($resultData['validationdata']);
			if (!empty($resultData['templatedata'])) $resultData['templatedata'] = ResultsModel::decompressData($resultData['templatedata']);
			if (!empty($resultData['inputdata'])) $resultData['inputdata'] = ResultsModel::decompressData($resultData['inputdata']);
		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $resultData;
	}

	public function getProjectProgressById(int $projectId) {

		try {

			$getData = $this->db->prepare("SELECT id,user_id,status,progress FROM projects WHERE id=:id");

			$getData->bindParam(':id', $projectId);

			$getData->execute();

			$resultData = $getData->fetch(PDO::FETCH_ASSOC);
		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $resultData;
	}

	public function getLatestCompleteRunId(int $projectId) {

		try {

			$getData = $this->db->prepare("SELECT run_id FROM giga.calculation_results WHERE project_id = :id and `type` ='object' ORDER BY id DESC LIMIT 1");

			$getData->bindParam(':id', $projectId);

			$getData->execute();

			$resultData = $getData->fetchColumn(0);
		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $resultData;
	}

	public function getLocationsByCountryId(int $country_id, string $type) {

		try {

			switch ($type) {
				case 'city':
					$sql = 'SELECT identifier,name,alt_name,lat,`long`,admin_name,admin_code,population,flag,area FROM locations WHERE country_id=:id and type=:type';
					break;
				case 'social_point':
					$sql = 'SELECT identifier,name,alt_name,lat,`long`,admin_name,admin_code,population,flag,area FROM locations WHERE country_id=:id and type=:type';
					break;
				default://hexagons
					$sql = 'SELECT identifier,lat,`long`,population,type,parent_identifier,rb,flag FROM locations WHERE country_id=:id and type=:type';
					break;
			}
			$getData = $this->db->prepare($sql);

			$getData->bindParam(':id', $country_id);
			$getData->bindParam(':type', $type);

			$getData->execute();

			$resultData = $getData->fetchAll(PDO::FETCH_NUM);
		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $resultData;
	}

	public function getTemplateDataByProjectById(int $projectId) {

		try {

			$getData = $this->db->prepare("SELECT templatedata FROM projects WHERE id=:id");

			$getData->bindParam(':id', $projectId);

			$getData->execute();

			$resultData = $getData->fetchColumn(0);
			$resultData = ResultsModel::decompressData($resultData);
		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $resultData;
	}

	/**
	 * @param int $projectIdentifier
	 * @return array $project
	 * @throws \Exception
	 */
	public function getProjectByIdentifier($projectIdentifier) {

		try {

			$getData = $this->db->prepare("SELECT * FROM projects WHERE identifier=:identifier");

			$getData->bindParam(':identifier', $projectIdentifier);

			$getData->execute();

			$resultData = $getData->fetch(PDO::FETCH_ASSOC);

		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $resultData;
	}

	/**
	 * @param $project
	 * @return bool
	 * @throws \Exception
	 */
	public function updateProject($project) {

		try {

			$update = $this->db->prepare("UPDATE projects SET name=:name, description=:description, access_level=:access_level WHERE id=:id");

			$update->bindParam(':id', $project['id']);
			$update->bindParam(':name', $project['name']);
			$update->bindParam(':description', $project['description']);
			$update->bindParam(':access_level', $project['access_level']);

			$update->execute();

		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return true;
	}


	public function updateProjectValidationData($projectId, $data) {

		try {

			$update = $this->db->prepare("UPDATE projects SET validationdata=:data WHERE id=:id");

			$data = ResultsModel::compressData($data);
			$update->bindParam(':id', $projectId);
			$update->bindParam(':data', $data, PDO::PARAM_LOB);

			$update->execute();

		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return true;
	}

	public function updateProjectTemplateData($projectId, $data) {

		try {

			$update = $this->db->prepare("UPDATE projects SET templatedata=:data,templatedata_upload_time=UTC_TIMESTAMP() WHERE id=:id");

			$data = ResultsModel::compressData($data);
			$update->bindParam(':id', $projectId);
			$update->bindParam(':data', $data, PDO::PARAM_LOB);

			$update->execute();

		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return true;
	}

	public function getInputDataByProjectID($projectId) {
		try {

			$getData = $this->db->prepare("SELECT inputdata FROM projects WHERE id=:id");

			$getData->bindParam(':id', $projectId);

			$getData->execute();

			$resultData = $getData->fetchColumn(0);
			$resultData = ResultsModel::decompressData($resultData);
		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $resultData;
	}

	public function updateProjectInputData($projectId, $data) {

		try {

			$update = $this->db->prepare("UPDATE projects SET inputdata=:data WHERE id=:id");

			$data = ResultsModel::compressData($data);
			$update->bindParam(':id', $projectId);
			$update->bindParam(':data', $data, PDO::PARAM_LOB);

			$update->execute();

		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return true;
	}

	public function updateProjectStatus($projectId, $status, $progress = 0, $status_data = null) {

		try {

			$update = $this->db->prepare("UPDATE projects SET status=:status,progress=:progress,statusdata=:statusdata WHERE id=:id");

			$update->bindParam(':id', $projectId);
			$update->bindParam(':status', $status);
			$update->bindParam(':progress', $progress);
			$update->bindParam(':statusdata', $status_data);

			$update->execute();

		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return true;
	}

	/**
	 * @param int $projectId
	 * @return boolean $status
	 * @throws \Exception
	 */
	public function deleteProject($projectId) {
		try {
			$del = $this->db->prepare("DELETE FROM projects_has_variables WHERE projects_id=:id");
			$del->bindParam(':id', $projectId);
			$del->execute();

			$del = $this->db->prepare("DELETE FROM calculation_results WHERE project_id=:id");
			$del->bindParam(':id', $projectId);
			$del->execute();

			$del = $this->db->prepare("DELETE FROM projects WHERE id=:id");
			$del->bindParam(':id', $projectId);
			$del->execute();

		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return true;
	}

	/**
	 * @param int $projectId
	 * @param int $variableId
	 * @return boolean $status
	 * @throws \Exception
	 */
	public function deleteVariableFromProject($projectId, $variableId) {
		try {

			$del = $this->db->prepare("DELETE FROM projects_has_variables WHERE projects_id=:projects_id AND variables_id=:variables_id");

			$del->bindParam(':projects_id', $projectId);
			$del->bindParam(':variables_id', $variableId);

			$del->execute();

		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return true;
	}

	/**
	 * @param int $userId
	 * @return array $projectsList
	 * @throws \Exception
	 */
	public function getProjectsListByUserId($userId, $mode = 'schools') {
		try {

			if ($userId === '*') {
				$getData = $this->db->prepare("SELECT p.id,p.name,p.country_id,p.description,p.status,p.created,p.updated,p.settings,p.progress,concat(u.firstname,' ',u.lastname) as owner
					FROM projects p JOIN users u on (p.user_id=u.id)
					WHERE p.mode=:mode");
			} else {
				$getData = $this->db->prepare("SELECT p.id,p.name,p.country_id,p.description,p.status,p.created,p.updated,p.settings,p.progress,concat(u.firstname,' ',u.lastname) as owner
					FROM projects p JOIN users u on (p.user_id=u.id)
					WHERE p.user_id=:user_id and p.mode=:mode");
				$getData->bindParam(':user_id', $userId);
			}

			$getData->bindParam(':mode', $mode);

			$getData->execute();

			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);

		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $resultData;
	}

	/**
	 * @param int $projectId
	 * @param int $technologyId
	 * @return array $variables
	 * @throws \Exception
	 */
	public function getVariablesForProject($projectId) {
		try {

			$getData = $this->db->prepare("SELECT variables.*, projects_has_variables.value FROM `variables` LEFT JOIN projects_has_variables ON (variables.id=projects_has_variables.variables_id AND projects_has_variables.projects_id=:projects_id) WHERE variables.class='project'");

			$getData->bindParam(':projects_id', $projectId);

			$getData->execute();

			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);

		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $resultData;
	}
	public function getVerifiedCountries() {
		try {

			$query = $this->db->prepare("SELECT * from country WHERE is_verified='1'");
			$query->execute();
			$data = $query->fetchAll(PDO::FETCH_ASSOC);
		} catch (\Exception $e) {
			throw new \Exception('Database error: '.$e->getMessage());
		}

		return $data;
	}

	public function getCountries() {
		try {
			$getData = $this->db->prepare("SELECT * FROM country ORDER BY country_name");
			$getData->execute();
			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);
		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $resultData;
	}

	public function getUserCountries($user_id) {
		try {
			$getData = $this->db->prepare("SELECT c.* FROM country AS c JOIN users_has_country uhc ON (c.id=uhc.country_id AND uhc.users_id=:user_id) ORDER BY c.country_name");
			$getData->bindParam(':user_id', $user_id);
			$getData->execute();
			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);
		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $resultData;
	}

	public function getInputTemplatesByProjectId($id) {
		try {
			$project = $this->getProjectById($id);
			$settings = json_decode($project['settings']);
			$getData = $this->db->prepare("SELECT id,class,template_type,filename,description FROM templates
				WHERE template_type LIKE 'INPUT%' AND filename NOT LIKE '%csv'
				AND is_broadband_required=:is_broadband_required
				AND is_bandwidth_calc_required=:is_bandwidth_calc_required
				AND is_lan_required=:is_lan_required");
			$is_bandwidth_calc_required = !empty($settings->is_bandwidth_calc_required) && $settings->is_bandwidth_calc_required !== 'provided' ? 1 : 0;
			$is_lan_required = !empty($settings->is_lan_required) && $settings->is_lan_required ? 1 : 0;
			$is_broadband_required = !empty($settings->is_broadband_required) && $settings->is_broadband_required ? 1 : 0;
			$getData->bindParam(':is_broadband_required', $is_broadband_required);
			$getData->bindParam(':is_bandwidth_calc_required', $is_bandwidth_calc_required);
			$getData->bindParam(':is_lan_required', $is_lan_required);
			$getData->execute();
			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);
		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $resultData;
	}

	public function getInputTemplates($flags) {
		try {
			$getData = $this->db->prepare("SELECT id,template_type,filename,description FROM templates
				WHERE template_type LIKE 'INPUT%' AND filename NOT LIKE '%csv'
				AND is_broadband_required=:is_broadband_required
				AND is_bandwidth_calc_required=:is_bandwidth_calc_required
				AND is_lan_required=:is_lan_required");
			$getData->bindParam(':is_broadband_required', $flags[0]);
			$getData->bindParam(':is_bandwidth_calc_required', $flags[1]);
			$getData->bindParam(':is_lan_required', $flags[2]);
			$getData->execute();
			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);
		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $resultData;
	}

	public function getInputTemplateByIdAPI($id) {
		try {
			$getData = $this->db->prepare("SELECT * FROM templates WHERE id=:id");
			$getData->bindParam(':id', $id);
			$getData->execute();
			$resultData = $getData->fetch(PDO::FETCH_ASSOC);
		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}

		return $resultData;
	}

	public function generateIdentifier($length = 8) {
		$chars = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
		$charsLength = strlen($chars);
		$identifier = '';
		for ($i = 0; $i < $length; $i++) {
			$identifier .= $chars[rand(0, $charsLength - 1)];
		}

		if ($this->validateIdentifierAlreadyExist($identifier)) {

			return $this->generateIdentifier();

		} else {

			return $identifier;
		}
	}


}
