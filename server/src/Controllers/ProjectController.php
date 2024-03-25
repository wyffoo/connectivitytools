<?php

namespace App\Controllers;
use App\Models\CalculationsModel;
use App\Models\LocationsModel;
use App\Models\OutputTemplates\GlobalOutputTemplate;
use App\Models\ProjectModel;
use App\Models\ResultsModel;
use App\Models\UserModel;
use phpDocumentor\Reflection\DocBlock\Description;
use Psr\Http\Message\ResponseInterface as Response;
use Slim\Logger;
use Slim\Psr7\Stream;
use Symfony\Component\Finder\Exception\AccessDeniedException;
use \Waavi\Sanitizer\Sanitizer;

class ProjectController extends Controller
{

	public function action_duplicateProjectAPI() {
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id']);

		if ($project['user_id'] !== $user->id) {
			$this->deny();
		}

		$projectModel->duplicateProject($project['id']);
		return $this->respondWithData('done');
	}

	public function action_downloadInputLocationsByProjectIdAPI() {
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id']);

		if ($user->role === UserModel::ROLE_ADMIN && $project['user_id'] !== $user->id) {
			$this->deny();
		}

		$type = $this->args['type'];
		if (!in_array($type, ['city','social_point','global','global_ext','hexagon_6','hexagon_7','hexagon_8','hexagon_9'])) {
			$this->deny();
		}

		$tmpname = tempnam('/tmp','locations_'.$project['country_id']);
		$locationsModel = new LocationsModel($this->db);
		$type = in_array($this->request->getAttribute('type'), ['city', 'social_point', 'global', 'global_ext', 'hexagon_6', 'hexagon_7', 'hexagon_8', 'hexagon_9']) ? $type : 'city';
		$query = $locationsModel->exportLocations($tmpname, $project['country_id'], $type);

		$global_output = new GlobalOutputTemplate($query);
		$global_output->generateLocationsCSV($tmpname, $type);

		$filename = urldecode($this->args['project_name']) . ' predefined locations ('.$type.')';
		$filename = ResultsModel::stripFilenameCharacters($filename).'.csv';

		//Server template to end user
		$fh = fopen($tmpname, 'r');
		fseek($fh, 0);
		$stream = new Stream($fh);
		session_write_close();
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

	public function action_downloadInputTemplateByIdAPI()
	{
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$template = $projectModel->getInputTemplateByIdAPI($this->args['id']);
		$type = $template['template_type'] === 'INPUTSIMPLE' ? 'simple' : 'extended';
		$filename = urldecode($this->args['project_name']) . ' data entry template (' . $type . ')';
		$filename = ResultsModel::stripFilenameCharacters($filename).'.xlsx';

		//Server template to end user
		$fh = fopen(realpath(__DIR__ . '/../Models/InputTemplates/' . $template['filename']), 'r');
		fseek($fh, 0);
		$stream = new Stream($fh);
		session_write_close();
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


	public function action_downloadInputLocationsDataByProjectIdAPI()
	{
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id']);

		if ($user->role === UserModel::ROLE_ADMIN && $project['user_id'] !== $user->id) {
			$this->deny();
		}

		//Process saved data
		$inputtemplate = $projectModel->getTemplateDataByProjectById($this->args['id']);

		//Save the results to a temporary file
		$saved_filename = tempnam('/tmp', 'templatedata' . $this->args['id']);
		file_put_contents($saved_filename, $inputtemplate);

		//Server template to end user
		$fh = fopen($saved_filename, 'r');
		fseek($fh, 0);
		$stream = new Stream($fh);
		$filename = ResultsModel::stripFilenameCharacters($project['name']).'.csv';
		session_write_close();
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

	public function action_downloadTemplateDataByProjectIdAPI()
	{
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id']);

		if ($project['user_id'] !== $user->id) {
			$this->deny();
		}

		//Process saved data
		$inputtemplate = $projectModel->getTemplateDataByProjectById($this->args['id']);

		//Save the results to a temporary file
		$saved_filename = tempnam('/tmp', 'templatedata' . $this->args['id']);
		file_put_contents($saved_filename, $inputtemplate);

		//Server template to end user
		$fh = fopen($saved_filename, 'r');
		fseek($fh, 0);
		$stream = new Stream($fh);
		$filename = ResultsModel::stripFilenameCharacters($project['name']).'.xlsx';
		session_write_close();
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

	public function action_addFilesToProjectAPI()
	{
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id']);

		if ($user->role === UserModel::ROLE_ADMIN && $project['user_id'] !== $user->id) {
			$this->deny();
		}

		$files = $this->request->getUploadedFiles();

		if (strpos($_ENV['ENV'],'prod') !== false && !isset($files['file'])) {
			throw new \Exception('File was not uploaded', 500);
		}

		try {
			if (strpos($_ENV['ENV'],'prod') !== false) {
				$blob = (string)$files['file']->getStream();
			} else {
				$blob = file_get_contents("/home/itsaryuk/Downloads/Telegram Desktop/new_inputs/SSD_5.csv");
			}

			if ($project['mode']==='schools') {
				$this->addSchoolsFile($project, $projectModel, $blob);
			} else {
				$this->addCountriesFile($project, $projectModel, $blob);
			}

		} catch (\Exception $e) {
			$this->logger->error($e->getMessage());
			throw new \Exception('File was not saved:' . $e->getMessage(), 500);
		}

		return $this->respondWithData('uploaded');
	}

	private function addSchoolsFile($project, $projectModel, $blob) {
		$projectModel->updateProjectTemplateData($project['id'], $blob);

		//Pre-validate template
		$calculation_parameters = json_decode($project['settings']);
		$calculationsModel = new CalculationsModel($this->db, $calculation_parameters);
		$template = $calculationsModel->loadTemplateData($blob, true);
		$invalid_objects = [];
		$i = 0;
		foreach ($template->objects as $obj) {
			if ($template->objects_validity[$i] === false) {
				$invalid_objects[] = [$obj[1],$obj[2]];
			}
			$i++;
		}

		$input_templates = $projectModel->getInputTemplatesByProjectId($project['id']);
		if (sizeof($template->objects) === 0) {
			$objects_validity = 0;
		} else {
			$objects_validity = sizeof($invalid_objects) > 0 ? (sizeof($invalid_objects) / sizeof($template->objects)) * 100 : 100;
		}

		$is_proper_template = $template->class === $input_templates[0]['class'] || $template->class === $input_templates[1]['class'];
		$validation_data = [
			'mode' => 'schools',
			'template_id' => $template->id,
			'template_class' => $template->class,
			'total_objects' => sizeof($template->objects),
			'valid_objects' => sizeof($template->objects) - sizeof($invalid_objects),
			'invalid_objects' => $invalid_objects,
			'objects_validity_percent' => $objects_validity,
			'is_proper_template' => $is_proper_template,
			'can_calculate' => $is_proper_template && $objects_validity >= 90
		];
		$projectModel->updateProjectValidationData($project['id'], json_encode($validation_data));
	}

	private function addCountriesFile($project, $projectModel, $blob) {
		$projectModel->updateProjectTemplateData($project['id'], $blob);

		//Pre-validate template
		$calculation_parameters = json_decode($project['settings']);
		$calculationsModel = new CalculationsModel($this->db, $calculation_parameters);
		$template = $calculationsModel->loadCountriesTemplateData($blob, true);
		$invalid_objects = [];
		$i = 0;
		foreach ($template->objects as $obj) {
			if ($template->objects_validity[$i] === false) {
				$invalid_objects[] = [$obj[1],$obj[2]];
			}
			$i++;
		}

		if (sizeof($template->objects) === 0) {
			$objects_validity = 0;
		} else {
			$objects_validity = sizeof($invalid_objects) > 0 ? (sizeof($invalid_objects) / sizeof($template->objects)) * 100 : 100;
		}

		if ($objects_validity > 0) {
			$validation_data = [
				'mode' => 'countries',
				'template_class' => 'GlobalInputTemplate',
				'data_type' => $template->type,
				'total_objects' => sizeof($template->objects),
				'valid_objects' => sizeof($template->objects) - sizeof($invalid_objects),
				'invalid_objects' => $invalid_objects,
				'objects_validity_percent' => $objects_validity,
				'is_proper_template' => true,
				'can_calculate' => $objects_validity >= 90
			];
			$projectModel->updateProjectValidationData($project['id'], json_encode($validation_data));
		}
	}

	public function action_getInputTemplatesByProjectIdAPI()
	{
		$projectModel = new ProjectModel($this->db);
		$templates = $projectModel->getInputTemplatesByProjectId($this->args['id']);
		return $this->respondWithData($templates);
	}

	public function action_getInputTemplatesAPI()
	{
		$flags = explode(',', $this->args['flags']);
		$projectModel = new ProjectModel($this->db);
		$templates = $projectModel->getInputTemplates($flags);
		return $this->respondWithData($templates);
	}

	/**
	 * @return Response
	 * @throws \Exception
	 */
	public function action_getCountriesListAPI()
	{
		$projectModel = new ProjectModel($this->db);
		$countries = $projectModel->getCountries();
		return $this->respondWithData($countries);
	}

	public function action_getVerifiedCountriesAPI() {
		if ($this->request->getAttribute('user')) {
			$user = $this->request->getAttribute('user');
			if ($user->role !== UserModel::ROLE_SYSADMIN) {
				$this->deny();
			}

			$locationsModel = new ProjectModel($this->db);
			$data = $locationsModel->getVerifiedCountries();
			$result = [];
			foreach ($data AS $item) {
				$result[] = ['user_id'=>$user->id,'val' => $item['id'], 'name' => $item['country_name']];
			}
			return $this->respondWithData($result);
		} else {

			return $this->respondWithData('');
		}

	}

	/**
	 * @return Response
	 * @throws \Exception
	 */
	public function action_getUserCountriesListAPI()
	{
		if ($this->request->getAttribute('user')) {
			$current_user = $this->request->getAttribute('user');
			$projectModel = new ProjectModel($this->db);
			if ($current_user->app_mode === 'global') {
				$countries = $projectModel->getVerifiedCountries();
			} else {
				$countries = $projectModel->getUserCountries($current_user->id);
			}
			return $this->respondWithData($countries);
		} else {
			throw new AccessDeniedException("Access denied.");
		}
	}

	/**
	 * @return mixed
	 */
	public function action_createProjectAPI()
	{

		if ($this->request->getAttribute('user')) {
			$current_user = $this->request->getAttribute('user');

			$data = $this->request->getParsedBody();

			$projectModel = new ProjectModel($this->db);

			$filters = [
				'country_id' => 'cast:integer',
				'name' => 'trim|escape|strip_tags',
				'description' => 'trim|escape|strip_tags'
			];

			$sanitizer  = new Sanitizer($data, $filters);
			$data = $sanitizer->sanitize();

			#if (isset($data['isCountriesMode'])  && $data['isCountriesMode'] === true) {
			#	$app_mode = 'countries';
			#} else if (isset($data['isSchoolsMode'])  && $data['isSchoolsMode'] === true) {
			#	$app_mode = 'schools';
			#} else {
			#	$app_mode = 'global';
			#}
			$app_mode = 'global'; // Default value
			if (strpos($_SERVER['HTTP_HOST'], 'countries') !== false) {
				$app_mode = 'countries';
			} elseif (strpos($_SERVER['HTTP_HOST'], 'schools') !== false) {
				$app_mode = 'schools';
			}
			$project = array(
				'name' => substr(remove_emoji($data['name']),0,240),
				'country_id' => $data['country_id'],
				'description' => empty($data['description']) ? '' : $data['description'],
				'identifier' => $projectModel->generateIdentifier(),
				'user_id' => $current_user->id,
				'mode' => $app_mode
			);

			//Remove project info and keep settings
			unset($data['name'], $data['country_id'], $data['description']);
			$project['settings'] = json_encode($data);

			try {
				$project_id = $projectModel->addProject($project);
				return $this->respondWithData(['project_id' => $project_id, 'status' => 'success']);
			} catch (\Exception $e) {
				$this->logger->error($e->getMessage());
				return $this->respondWithData(['status' => 'error']);
			}
		} else {
			throw new AccessDeniedException("Access denied.");
		}

	}

	/**
	 * @param \Slim\Http\Request $request
	 * @param \Slim\Http\Response $response
	 * @param string $name
	 * @param string $description
	 * @param array $objects
	 * @param array $technologies
	 * @param boolean $isPublic
	 * @return \Slim\Http\Response
	 * In the incoming $_POST:
	 */
	public function updateProjectAPI(\Slim\Http\Request $request, \Slim\Http\Response $response, $args)
	{

		if ($this->request->getAttribute('user')) {
			$current_user = $this->request->getAttribute('user');

			$projectModel = new ProjectModel($this->db);

			$project = $projectModel->getProjectById($args['id']);

			if ($current_user->role !== UserModel::ROLE_SYSADMIN && $current_user->id === $project['user_id']) {

				$data = $this->request->getParsedBody();

				$changed = false;
				if (!empty($data['name']) && $project['name'] != $data['name']) {

					$project['name'] = $data['name'];
					$changed = true;

				}

				if (!empty($data['description']) && $project['description'] != $data['description']) {

					$project['description'] = $data['description'];
					$changed = true;

				}

				// Check accesslevel & update if need
				if (!empty($data['isPublic'])) {
					if (strtolower($data['isPublic']) === 'true') {
						if ($project['access_level'] != 3) {
							$project['access_level'] = 3;
							$changed = true;
						}
					} else {
						if ($project['access_level'] != 0) {
							$project['access_level'] = 0;
							$changed = true;
						}
					}
				}

				if ($changed === true) {
					$projectModel->updateProject($project);
				}

				// Update objects for project
				$new_objects = $data['objects'];

				$objectModel = new ObjectModel($this->container);

				// Select all existing objects
				$old_objects = $objectModel->getObjectsListByProjectId($project['id'])['rows'];

				if (!empty($new_objects)) {
					foreach ($new_objects as $new_row) {
						$found = false;
						foreach ($old_objects as $key => $old_row) {
							if ($new_row['name'] == $old_row['name']) { //Object found
								$found = true;
								if ($new_row['designer']['id'] != $old_row['user_id']) {
									$object = $old_row;
									$object['user_id'] = $new_row['designer']['id'];
									$objectModel->updateObject($object);
								}
								unset($old_objects[$key]);
							}
						}
						if ($found === false) {
							$object = array(
								'name' => $new_row['name'],
								'project_id' => $project['id'],
								'user_id' => $new_row['designer']['id']
							);
							$objectId = $objectModel->addObject($object);
						}
					}
					foreach ($old_objects as $key => $old_row) {
						$objectModel->deleteObject($old_row['id']);
					}

				} else { // $objects in POST is empty
					// Delete all old routes
					foreach ($old_objects as $key => $old_row) {
						$objectModel->deleteObject($old_row['id']);
					}
				}

				// TODO: Update technologies for project
				$new_techs = $data['technologies'];

				$old_techs = $projectModel->getTechnologiesForProject($project['id']);

				if (!empty($new_techs)) {
					$technologyModel = new TechnologyModel($this->container);

					foreach ($new_techs as $new_row) {
						$found = false;
						foreach ($old_techs as $key => $old_row) {
							if ($new_row['id'] == $old_row['id']) {
								$found = true;
								unset($old_techs[$key]);
							}
						}
						if ($found === false) {
							if (!$technologyModel->isTechnologyExist($new_row['id'])) {
								throw new Exception("Sorry, but this technology is absent.", 500);
							}

							// If technology is exist
							$linkId = $projectModel->addTechnologyToProject($project['id'], $new_row['id']);
						}
					}

					foreach ($old_techs as $row) {
						$projectModel->deleteTechnologyFromProject($project['id'], $row['technologies_id']);
					}

				} else {
					// Delete all tehnologies from project
					foreach ($old_techs as $row) {
						$projectModel->deleteTechnologyFromProject($project['id'], $row['technologies_id']);
					}
				}

				return $this->respondWithData(['status' => 'success']);

			} else {

				throw new \Exception("Access denied.", 500);
			}
		} else {

			throw new \Exception("Access denied.", 500);
		}

	}

	public function action_getProjectProgressByIdAPI()
	{
		if ($this->request->getAttribute('user')) {
			$current_user = $this->request->getAttribute('user');

			$projectModel = new ProjectModel($this->db);

			$project = $projectModel->getProjectProgressById($this->args['id']);

			if ($current_user->role === UserModel::ROLE_SYSADMIN || $current_user->id === $project['user_id']) {

				unset($project['user_id']);

				return $this->respondWithData($project);

			} else {
				throw new \Exception("Access denied.", 500);
			}

		} else {

			throw new \Exception("Access denied.", 500);
		}

	}

	public function action_getProjectByIdAPI()
	{

		if ($this->request->getAttribute('user')) {
			$current_user = $this->request->getAttribute('user');

			$projectModel = new ProjectModel($this->db);

			$project = $projectModel->getProjectById($this->args['id']);

			if ($current_user->role === UserModel::ROLE_SYSADMIN || $current_user->id === $project['user_id']) {

				unset($project['user_id']);
				unset($project['identifier']);

				return $this->respondWithData($project);

			} else {

				throw new \Exception("Access denied.", 500);
			}

		} else {

			throw new \Exception("Access denied.", 500);
		}

	}

	public function action_deleteProjectAPI()
	{
		if ($this->request->getAttribute('user')) {
			$current_user = $this->request->getAttribute('user');

			$projectModel = new ProjectModel($this->db);

			$project = $projectModel->getProjectById($this->args['id']);

			if ($current_user->id === $project['user_id']) {

				$projectModel->deleteProject($project['id']);

				return $this->respondWithData(['status' => 'success']);

			} else {

				throw new \Exception("Access denied.", 500);
			}

		} else {

			throw new \Exception("Access denied.", 500);
		}
	}

	public function action_getProjectsListAPI()
	{
		if ($this->request->getAttribute('user')) {
			$user = $this->request->getAttribute('user');

			$projectModel = new ProjectModel($this->db);

			$projectsList = $projectModel->getProjectsListByUserId($user->role === UserModel::ROLE_SYSADMIN ? '*' : $user->id, $user->app_mode);

			return $this->respondWithData($projectsList);

		} else {

			return $this->respondWithData('');
		}

	}

}
