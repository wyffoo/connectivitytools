<?php

namespace App\Controllers;

use App\Models\CalculationsModel;
use App\Models\ProjectModel;
use App\Models\ResultsModel;
use App\Models\UserModel;
use PhpAmqpLib\Channel\AMQPChannel;
use PhpAmqpLib\Connection\AbstractConnection;
use Psr\Http\Message\ResponseInterface as Response;
use PhpAmqpLib\Message\AMQPMessage;

class CalculationController extends Controller {

	/**
	 * @return Response
	 */
	public function action_calculateBandwidthAPI() {
		return $this->respondWithData();
	}

	public function action_stopProjectAPI() {
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id'], false);

		if (($user->app_mode === 'schools' && $project['user_id'] !== $user->id)
			|| (in_array($user->app_mode,['global','countries']) && $user->role === UserModel::ROLE_ADMIN && $project['user_id'] !== $user->id)) {
			$this->deny();
		}

		try {
			if (in_array($project['status'], ['in-progress','waiting'])) {
				$projectModel->updateProjectStatus($this->args['id'], 'stop');
			} else {
				$projectModel->updateProjectStatus($this->args['id'], 'not-calculated');
			}
		} catch (\Exception $e) {
			$this->logger->error($e);
			return $this->respondWithData('error');
		}

		return $this->respondWithData('done');
	}

	public function action_calculateProjectAPI(){
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id'], true);

		if (
			($user->app_mode === 'schools' && $project['user_id'] === $user->id)
			||
			(in_array($user->app_mode,['global','countries']) && $user->role === UserModel::ROLE_ADMIN && $project['user_id'] === $user->id)
			||
			(in_array($user->app_mode,['global','countries']) && $user->role === UserModel::ROLE_SYSADMIN)
		) {
			try {
				if ($user->role === UserModel::ROLE_ADMIN) {
					//Pre-waiting
				} else {

				}
				$templatedata = $projectModel->getTemplateDataByProjectById($this->args['id']);
				$model = new CalculationsModel($this->db, json_decode($project['settings']));
				$validation = json_decode($project['validationdata']);
				//Load input template
				$template = $model->loadTemplateData($templatedata, false, $validation);
				//Prepare variables and objects for calculation
				$input_data = $model->prepareDataForCalculations($project, $template);
				if (in_array($project['mode'],['global','countries'])) {
					$input_data['data_type'] = !empty($template->type) ? $template->type : $validation->data_type;
				}
				$templatedata = null; $validation = null;
				$projectModel->updateProjectInputData($project['id'], json_encode($input_data));
				$input_data = null;
				if ($user->role === UserModel::ROLE_ADMIN && in_array($user->app_mode,['countries'])) {
					$projectModel->updateProjectStatus($this->args['id'], 'data-ready');
				} else {
					if ($this->pushProjectToCalculationsQueue($project)) {
						$projectModel->updateProjectStatus($this->args['id'], 'waiting');
					}
				}
			} catch (\Exception $e) {
				$this->logger->error($e);
				return $this->respondWithData('error');
			}

			return $this->respondWithData('scheduled');
		} else {
			$this->deny();
		}

	}

	public function action_calculateProjectTopologyAPI(){
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id'], true);

		if (($user->app_mode === 'schools' && $project['user_id'] !== $user->id) || (in_array($user->app_mode,['global','countries']) && $user->role !== UserModel::ROLE_SYSADMIN)) {
			$this->deny();
		}

		try {
			$templatedata = $projectModel->getTemplateDataByProjectById($this->args['id']);
			$model = new CalculationsModel($this->db, json_decode($project['settings']));
			$validation = json_decode($project['validationdata']);
			//Load input template
			$template = $model->loadTemplateData($templatedata, false, $validation);
			//Prepare variables and objects for calculation
			$input_data = $model->prepareDataForCalculations($project, $template);
			if (in_array($project['mode'],['global','countries'])) {
				$input_data['data_type'] = !empty($template->type) ? $template->type : $validation->data_type;
			}
			$projectModel->updateProjectInputData($project['id'], json_encode($input_data));
			$projectModel->updateProjectStatus($this->args['id'], 'waiting');
			$this->pushProjectToCalculationsQueue($project, 'topology');
		} catch (\Exception $e) {
			$this->logger->error($e);
			return $this->respondWithData('error');
		}

		return $this->respondWithData('scheduled');
	}

	private function pushProjectToCalculationsQueue($project, $mode = 'complete'){
		/**
		 * @var AbstractConnection
		 */
		$connection = $this->container->get('rabbitmq');
		if ($connection === null) {
			$connection = $this->container->get('sqs');
			$result = $connection->sendMessage([
				'MessageBody' => json_encode(['project_id'=>$project['id'],'mode'=>$mode]),
				'QueueUrl' => $this->container->get('settings')['sqs']['url'],
				'MessageGroupId' => 'v2',
				'MessageDeduplicationId' => 'project-'.$project['id']
			]);
			return $result !== 'error';
		} else {
			$channel = $connection->channel();
			$channel->queue_declare('giga_calculation_requests_old', false, true, false, false);
			$msg = new AMQPMessage(json_encode(['project_id' => $project['id'], 'mode' => $mode]), array('delivery_mode' => AMQPMessage::DELIVERY_MODE_PERSISTENT));
			$channel->exchange_declare('logs', 'fanout', false, false, false);
			$channel->basic_publish($msg, '', 'giga_calculation_requests_old');
			$channel->close();
			$connection->close();
			return true;
		}
	}

}
