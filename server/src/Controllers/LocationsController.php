<?php

namespace App\Controllers;

use App\Models\LocationsModel;
use App\Models\OutputTemplates\GlobalOutputTemplate;
use App\Models\ResultsModel;
use App\Models\UserModel;
use phpDocumentor\Reflection\DocBlock\Description;
use Psr\Http\Message\ResponseInterface as Response;
use Slim\Logger;
use Slim\Psr7\Stream;
use Symfony\Component\Finder\Exception\AccessDeniedException;
use \Waavi\Sanitizer\Sanitizer;

class LocationsController extends Controller {

	public function action_exportDatasetAPI() {
		$user = $this->authorize();

		if ($user->role !== UserModel::ROLE_SYSADMIN) {
			$this->deny();
		}

		try {
			$country_id = (int)$this->request->getAttribute('country_id');
			$tmpname = tempnam('/tmp','dataset');
			$locationsModel = new LocationsModel($this->db);
			$type = in_array($this->request->getAttribute('type'), ['city', 'social_point', 'global', 'global_ext', 'hexagon_6', 'hexagon_7', 'hexagon_8', 'hexagon_9']) ? $this->request->getAttribute('type') : 'city';
			$locationsModel->exportLocations($tmpname, $country_id, $type);
		} catch (\Exception $e) {
			$this->logger->error($e->getMessage());
			throw new \Exception('File was not saved:' . $e->getMessage(), 500);
		}

		$filename = "c{$country_id} {$type} dataset";
		$filename = ResultsModel::stripFilenameCharacters($filename) . '.csv';

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

	public function action_importDatasetAPI() {
		$user = $this->authorize();

		if ($user->role !== UserModel::ROLE_SYSADMIN) {
			$this->deny();
		}

		$files = $this->request->getUploadedFiles();

		if (strpos($_ENV['ENV'],'prod') !== false && !isset($files['file'])) {
			throw new \Exception('File was not uploaded', 500);
		}

		try {
			if (strpos($_ENV['ENV'],'prod') !== false) {
				/**
				 * $files \Slim\Psr7\UploadedFile
				 */
				$filename = tempnam('/tmp','location');
				$files['file']->moveTo($filename);
			} else {
				$filename = "/home/itsaryuk/Downloads/Telegram Desktop/KEN_hex8.csv";
			}
			$locationsModel = new LocationsModel($this->db);
			$type = in_array($this->request->getAttribute('type'), ['city', 'social_point', 'global', 'global_ext', 'hexagon_6', 'hexagon_7', 'hexagon_8', 'hexagon_9']) ? $this->request->getAttribute('type') : 'city';
			$locationsModel->importLocations($filename, $this->request->getAttribute('country_id'), $type);
		} catch (\Exception $e) {
			$this->logger->error($e->getMessage());
			throw new \Exception('File was not saved:' . $e->getMessage(), 500);
		}

		return $this->respondWithData('uploaded');
	}

	public function action_getAvailableCountriesAPI() {
		if ($this->request->getAttribute('user')) {
			$user = $this->request->getAttribute('user');
			if ($user->role !== UserModel::ROLE_SYSADMIN) {
				$this->deny();
			}

			$locationsModel = new LocationsModel($this->db);
			$data = $locationsModel->getAvailableCountries();
			$result = [];
			foreach ($data AS $item) {
				$result[] = ['val'=>$item['id'],'name'=>$item['country_name']];
			}
			return $this->respondWithData($result);

		} else {

			return $this->respondWithData('');
		}

	}

	public function action_getLocationsAPI() {
		if ($this->request->getAttribute('user')) {
			$user = $this->request->getAttribute('user');
			if ($user->role !== UserModel::ROLE_SYSADMIN) {
				$this->deny();
			}

			$params = $this->request->getQueryParams();
			$locationsModel = new LocationsModel($this->db);
			$type = isset($params['type']) && in_array($params['type'], ['city', 'social_point', 'global', 'global_ext', 'hexagon_6', 'hexagon_7', 'hexagon_8', 'hexagon_9']) ? $params['type'] : 'city';
			//$search = isset($params['search'])
			$country_id = isset($params['country_id']) ? (int)$params['country_id'] : null;
			$locations = $locationsModel->getLocations($country_id, $type, $params['limit'], $params['offset'], $params['order_by'] ?? '');

			return $this->respondWithData([
				'meta' => ['type' => $type, 'total' => $locations['total']],
				'data' => empty($locations['data']) ? [] : $locations['data']
			]);

		} else {

			return $this->respondWithData('');
		}

	}

}
