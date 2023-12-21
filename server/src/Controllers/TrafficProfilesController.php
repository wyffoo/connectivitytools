<?php

namespace App\Controllers;

use App\Models\ProjectModel;
use App\Models\TrafficProfilesModel;
use Psr\Http\Message\ResponseInterface as Response;
use Slim\Logger;
use Slim\Psr7\Stream;
use Symfony\Component\Finder\Exception\AccessDeniedException;
use Waavi\Sanitizer\Sanitizer;

class TrafficProfilesController extends Controller
{

	public function action_getProfileAPI() {
		$tpModel = new TrafficProfilesModel($this->db);
		$user = $this->authorize();

		try {
			$data = $tpModel->getProfile($user->id, $this->args['id']);
			return $this->respondWithData($data);
		} catch (\Exception $e) {
			$this->logger->error($e->getMessage());
			return $this->respondWithData(['status' => 'error']);
		}
	}

	public function action_updateProfileAPI() {
		$tpModel = new TrafficProfilesModel($this->db);
		$user = $this->authorize();
		$data = $this->request->getParsedBody();

		$filters = [
			'name' => 'trim|escape|strip_tags',
			'quality_level' => 'trim|escape|uppercase'
		];

		$sanitizer  = new Sanitizer($data, $filters);
		$data = $sanitizer->sanitize();

		$profile = [
			'name' => !empty($data['name']) ? $data['name'] : uniqid(),
			'user_id' => $user->id,
			'quality_level' => $data['quality_level']
		];

		$profile['records'] = [];
		foreach ($data['records'] AS $record) {
			if ($data['include_default_services'] === false && $record[3] === 0) continue;
			$profile['records'][] = $record;
		}

		try {
			$profile_id = $tpModel->updateProfile($this->args['id'], $profile);
			return $this->respondWithData(['status' => 'success', 'profile_id' => $profile_id]);
		} catch (\Exception $e) {
			$this->logger->error($e->getMessage());
			return $this->respondWithData(['status' => 'error']);
		}
	}

	public function action_createProfileAPI() {
		$tpModel = new TrafficProfilesModel($this->db);
		$user = $this->authorize();
		$data = $this->request->getParsedBody();

		$filters = [
			'name' => 'trim|escape|strip_tags',
			'quality_level' => 'trim|escape|uppercase'
		];

		$sanitizer  = new Sanitizer($data, $filters);
		$data = $sanitizer->sanitize();

		$profile = [
			'name' => !empty($data['name']) ? $data['name'] : uniqid(),
			'user_id' => $user->id,
			'quality_level' => $data['quality_level']
		];

		$profile['records'] = [];
		foreach ($data['records'] AS $record) {
			if ($data['include_default_services'] === false && $record[3] === 0) continue;
			$profile['records'][] = $record;
		}

		try {
			$profile_id = $tpModel->addProfile($profile);
			return $this->respondWithData(['status' => 'success', 'profile_id' => $profile_id]);
		} catch (\Exception $e) {
			$this->logger->error($e->getMessage());
			return $this->respondWithData(['status' => 'error']);
		}
	}

	public function action_deleteProfileAPI() {
		$tpModel = new TrafficProfilesModel($this->db);
		$user = $this->authorize();

		$service = [
			'id' => (int) $this->args['id'],
			'user_id' => $user->id,
		];

		try {
			$tpModel->deleteProfile($service);
			return $this->respondWithData(['status' => 'success']);
		} catch (\Exception $e) {
			$this->logger->error($e->getMessage());
			return $this->respondWithData(['status' => 'error']);
		}
	}

	public function action_deleteServiceAPI() {
		$tpModel = new TrafficProfilesModel($this->db);
		$user = $this->authorize();

		$service = [
			'id' => (int) $this->args['id'],
			'user_id' => $user->id,
		];

		try {
			$tpModel->deleteService($service);
			return $this->respondWithData(['status' => 'success']);
		} catch (\Exception $e) {
			$this->logger->error($e->getMessage());
			return $this->respondWithData(['status' => 'error']);
		}
	}

	public function action_updateServiceAPI() {
		$tpModel = new TrafficProfilesModel($this->db);
		$user = $this->authorize();
		$data = $this->request->getParsedBody();

		foreach ($data AS $input) {
			if ($input === '') {
				return $this->respondWithData(['status' => 'error']);
			}
		}

		$filters = [
			'service_name' => 'trim|escape|strip_tags',
		];

		$sanitizer  = new Sanitizer($data, $filters);
		$data = $sanitizer->sanitize();


		$service = [
			'id' => (int) $data['id'],
			'service_name' => $data['service_name'],
			'user_id' => $user->id,
			'bitrate_high' => (float)abs($data['bitrate_high']),
			'bitrate_low' => (float)abs($data['bitrate_low']),
			'bitrate_medium' => (float)abs($data['bitrate_medium']),
			'datavolume_high' => (float)abs($data['datavolume_high']),
			'datavolume_low' => (float)abs($data['datavolume_low']),
			'datavolume_medium' => (float)abs($data['datavolume_medium']),
			'intensity_high' => (float)abs($data['intensity_high']),
			'intensity_low' => (float)abs($data['intensity_low']),
			'intensity_medium' => (float)abs($data['intensity_medium']),
			'latency_high' => (float)abs($data['latency_high']),
			'latency_low' => (float)abs($data['latency_low']),
			'latency_medium' => (float)abs($data['latency_medium'])
		];

		try {
			$tpModel->updateService($service);
			return $this->respondWithData(['status' => 'success']);
		} catch (\Exception $e) {
			$this->logger->error($e->getMessage());
			return $this->respondWithData(['status' => 'error']);
		}
	}

	public function action_createServiceAPI() {
		$tpModel = new TrafficProfilesModel($this->db);
		$user = $this->authorize();
		$data = $this->request->getParsedBody();

		foreach ($data AS $input) {
			if ($input === '') {
				return $this->respondWithData(['status' => 'error']);
			}
		}

		if (empty($data['service_name'])) {
			return $this->respondWithData(['status' => 'error']);
		}

		$filters = [
			'service_name' => 'trim|escape|strip_tags',
		];

		$sanitizer  = new Sanitizer($data, $filters);
		$data = $sanitizer->sanitize();

		$service = [
			'service_name' => $data['service_name'],
			'user_id' => $user->id,
			'bitrate_high' => (float)abs($data['bitrate_high']),
			'bitrate_low' => (float)abs($data['bitrate_low']),
			'bitrate_medium' => (float)abs($data['bitrate_medium']),
			'datavolume_high' => (float)abs($data['datavolume_high']),
			'datavolume_low' => (float)abs($data['datavolume_low']),
			'datavolume_medium' => (float)abs($data['datavolume_medium']),
			'intensity_high' => (float)abs($data['intensity_high']),
			'intensity_low' => (float)abs($data['intensity_low']),
			'intensity_medium' => (float)abs($data['intensity_medium']),
			'latency_high' => (float)abs($data['latency_high']),
			'latency_low' => (float)abs($data['latency_low']),
			'latency_medium' => (float)abs($data['latency_medium'])
		];

		try {
			$service_id = $tpModel->addService($service);
			return $this->respondWithData(['service_id' => $service_id, 'status' => 'success']);
		} catch (\Exception $e) {
			$this->logger->error($e->getMessage());
			return $this->respondWithData(['status' => 'error']);
		}
	}

	public function action_getProfileRecordsAPI()
	{
		$tpModel = new TrafficProfilesModel($this->db);
		$user = $this->authorize();
		try {
			$list = $tpModel->getRecordsList($user->id, $this->args['id']);
			return $this->respondWithData($list);
		} catch (\Exception $e) {
			$this->logger->error($e->getMessage());
			return $this->respondWithData(['status' => 'error']);
		}
	}

	public function action_getCustomListAPI()
	{
		$tpModel = new TrafficProfilesModel($this->db);
		$user = $this->authorize();
		$list = $tpModel->getCustomProfilesList($user->id);
		$result = [];
		foreach ($list as $tp) {
			$result[$tp['id']] = ['id' => $tp['id'], 'name' => $tp['profile_name'], 'ql' => $tp['quality_level']];
		}
		return $this->respondWithData($result);
	}

	/**
	 * @return Response
	 * @throws \Exception
	 */
	public function action_getDefaultListAPI()
	{
		$tpModel = new TrafficProfilesModel($this->db);
		$user = $this->authorize();
		$list = $tpModel->getDefaultProfilesList($user->preferred_language);
		$result = [];
		foreach ($list as $tp) {
			$result[$tp['id']] = [$tp['profile_name'],$tp['description'],$tp['quality_level']];
		}
		return $this->respondWithData($result);
	}

	public function action_getTrafficSourcesListAPI()
	{
		$tpModel = new TrafficProfilesModel($this->db);
		$user = $this->authorize();
		$list = $tpModel->getTrafficSourcesList($user->preferred_language);
		$result = [];
		foreach ($list as $tp) {
			$result[] = [$tp['id'],$tp['name'],$tp['defaultset']];
		}
		return $this->respondWithData($result);
	}

	public function action_getServicesListAPI()
	{
		$tpModel = new TrafficProfilesModel($this->db);
		$user = $this->authorize();
		$list = $tpModel->getServicesList($user->id, $user->preferred_language);
		return $this->respondWithData($list);
	}



}
