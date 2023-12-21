<?php

namespace App\Models;

use PDO;

class TrafficProfilesModel {

	protected $container;

	protected $db;

	public function __construct($db) {
		$this->db = $db;
	}

	public function deleteProfile($profile) {
		try {

			$this->db->beginTransaction();

			$query = $this->db->prepare("DELETE FROM profile_records WHERE traffic_profiles_id=?");
			$query->bindParam(1, $profile['id']);
			$query->execute();

			$query = $this->db->prepare("DELETE FROM traffic_profiles WHERE id=? AND users_id=? AND defaultset=0");
			$query->bindParam(1, $profile['id']);
			$query->bindParam(2, $profile['user_id']);
			$query->execute();

			$this->db->commit();

		} catch (\Exception $e) {
			$this->db->rollBack();
			throw new \Exception('Database error: '.$e->getMessage());
		}

		return true;
	}


	public function getProfile($user_id, $profile_id) {
		try {
			$query = $this->db->prepare("SELECT id,profile_name,quality_level
				FROM traffic_profiles WHERE id=? AND users_id=? AND defaultset=0");
			$query->bindParam(1, $profile_id);
			$query->bindParam(2, $user_id);

			$query->execute();
			$resultData = $query->fetch(PDO::FETCH_ASSOC);
		} catch (\Exception $e) {
			throw new \Exception('Database error: '.$e->getMessage());
		}

		return $resultData;
	}

	public function updateProfile($profile_id, $profile) {
		try {

			$this->db->beginTransaction();
			$query = $this->db->prepare("UPDATE traffic_profiles SET profile_name=?, quality_level=?
				WHERE id=? AND users_id=? AND defaultset=0");

			$query->bindParam(1, $profile['name']);
			$query->bindParam(2, $profile['quality_level']);
			$query->bindParam(3, $profile_id);
			$query->bindParam(4, $profile['user_id']);

			$query->execute();

			$this->insertProfileRecords($profile_id, $profile['records']);

			$this->db->commit();

		} catch (\Exception $e) {
			$this->db->rollBack();
			throw new \Exception('Database error: '.$e->getMessage());
		}

		return $profile_id;
	}

	public function addProfile($profile) {
		try {

			$this->db->beginTransaction();
			$query = $this->db->prepare("INSERT INTO traffic_profiles
				( profile_name, quality_level, defaultset, users_id, project_id ) VALUES ( ?, ?, 0, ?, 0)");

			$query->bindParam(1, $profile['name']);
			$query->bindParam(2, $profile['quality_level']);
			$query->bindParam(3, $profile['user_id']);

			$query->execute();
			$profile_id = (int)$this->db->lastInsertId();

			$this->insertProfileRecords($profile_id, $profile['records']);

			$this->db->commit();

		} catch (\Exception $e) {
			$this->db->rollBack();
			throw new \Exception('Database error: '.$e->getMessage());
		}

		return $profile_id;
	}

	private function insertProfileRecords($profile_id, $records) {
		try {

			foreach ($records AS $record) {
				$query = $this->db->prepare("INSERT INTO profile_records (traffic_profiles_id,traffic_sources_id,services_id,using_level)
				VALUES (?,?,?,?) ON DUPLICATE KEY UPDATE using_level=?");

				$ql = strtoupper($record[2]);
				$query->bindParam(1, $profile_id);
				$query->bindParam(2, $record[0]);
				$query->bindParam(3, $record[1]);
				$query->bindParam(4, $ql);
				$query->bindParam(5, $ql);

				$query->execute();
			}
		} catch (\Exception $e) {
			throw new \Exception('Database error: '.$e->getMessage());
		}
	}

	public function deleteService($service) {
		try {

			$query = $this->db->prepare("DELETE FROM profile_records WHERE services_id=?");
			$query->bindParam(1, $service['id']);
			$query->execute();

			$query = $this->db->prepare("DELETE FROM services WHERE id=? AND users_id=? AND defaultset=0");
			$query->bindParam(1, $service['id']);
			$query->bindParam(2, $service['user_id']);
			$query->execute();

		} catch (\Exception $e) {
			throw new \Exception('Database error: '.$e->getMessage());
		}

		return true;
	}

	public function updateService($service) {
		try {

			$query = $this->db->prepare("UPDATE services
				SET service_name=?,
				 bitrate_high=?, bitrate_low=?, bitrate_medium=?,
				 datavolume_high=?, datavolume_low=?, datavolume_medium=?,
				 intensity_high=?, intensity_low=?, intensity_medium=?,
				 latency_high=?, latency_medium=?, latency_low=? WHERE id=? AND users_id=?");

			$query->bindParam(1, $service['service_name']);
			$query->bindParam(2, $service['bitrate_high']);
			$query->bindParam(3, $service['bitrate_low']);
			$query->bindParam(4, $service['bitrate_medium']);
			$query->bindParam(5, $service['datavolume_high']);
			$query->bindParam(6, $service['datavolume_low']);
			$query->bindParam(7, $service['datavolume_medium']);
			$query->bindParam(8, $service['intensity_high']);
			$query->bindParam(9, $service['intensity_low']);
			$query->bindParam(10, $service['intensity_medium']);
			$query->bindParam(11, $service['latency_high']);
			$query->bindParam(12, $service['latency_medium']);
			$query->bindParam(13, $service['latency_low']);
			$query->bindParam(14, $service['id']);
			$query->bindParam(15, $service['user_id']);

			$query->execute();

		} catch (\Exception $e) {
			throw new \Exception('Database error: '.$e->getMessage());
		}

		return true;
	}

	public function addService($service) {
		try {

			$insert = $this->db->prepare("INSERT INTO services
				( service_name, defaultset, users_id,
				 bitrate_high, bitrate_low, bitrate_medium,
				 datavolume_high, datavolume_low, datavolume_medium,
				 intensity_high, intensity_low, intensity_medium,
				 latency_high, latency_medium, latency_low) VALUES ( ?, 0, ?, ?,?,?, ?,?,?, ?,?,?, ?,?,? )");

			$insert->bindParam(1, $service['service_name']);
			$insert->bindParam(2, $service['user_id']);
			$insert->bindParam(3, $service['bitrate_high']);
			$insert->bindParam(4, $service['bitrate_low']);
			$insert->bindParam(5, $service['bitrate_medium']);
			$insert->bindParam(6, $service['datavolume_high']);
			$insert->bindParam(7, $service['datavolume_low']);
			$insert->bindParam(8, $service['datavolume_medium']);
			$insert->bindParam(9, $service['intensity_high']);
			$insert->bindParam(10, $service['intensity_low']);
			$insert->bindParam(11, $service['intensity_medium']);
			$insert->bindParam(12, $service['latency_high']);
			$insert->bindParam(13, $service['latency_medium']);
			$insert->bindParam(14, $service['latency_low']);

			$insert->execute();

		} catch (\Exception $e) {
			throw new \Exception('Database error: '.$e->getMessage());
		}

		return (int)$this->db->lastInsertId();
	}


	public function getRecordsList($user_id, $tp_id) {
		try {
			$getData = $this->db->prepare("SELECT pr.traffic_sources_id AS ts_id, pr.services_id AS s_id, pr.using_level AS level, tp.defaultset FROM profile_records pr JOIN traffic_profiles tp ON (tp.id=pr.traffic_profiles_id)
				WHERE tp.users_id = ? AND traffic_profiles_id = ?");
			$getData->bindParam(1, $user_id);
			$getData->bindParam(2, $tp_id);
			$getData->execute();
			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);
		} catch (\Exception $e) {
			throw new \Exception('Database error: '.$e->getMessage());
		}

		return $resultData;
	}

	public function getCustomProfilesList($user_id) {
		try {
			$getData = $this->db->prepare("SELECT id,profile_name,quality_level
				FROM traffic_profiles WHERE defaultset=0 AND users_id=?");
			$getData->bindParam(1, $user_id);
			$getData->execute();
			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);
		} catch (\Exception $e) {
			throw new \Exception('Database error: '.$e->getMessage());
		}

		return $resultData;
	}

	public function getDefaultProfilesList($lang='en') {
		try {
			$getData = $this->db->prepare("SELECT id,profile_name,quality_level,description_{$lang} AS description
				FROM traffic_profiles WHERE defaultset=1");
			$getData->execute();
			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);
		} catch (\Exception $e) {
			throw new \Exception('Database error: '.$e->getMessage());
		}

		return $resultData;
	}

	public function getTrafficSourcesList($lang='en') {
		try {
			$getData = $this->db->prepare("SELECT id,trafficsource_name AS name, 1 AS 'defaultset' FROM traffic_sources");
			$getData->execute();
			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);
		} catch (\Exception $e) {
			throw new \Exception('Database error: '.$e->getMessage());
		}

		return $resultData;
	}

	public function getServicesList($user_id, $lang='en') {
		try {
			$getData = $this->db->prepare("SELECT * FROM services WHERE defaultset='1' OR (defaultset!='1' AND users_id=:user_id)");
			$getData->bindParam(':user_id', $user_id);
			$getData->execute();
			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);
		} catch (\Exception $e) {
			throw new \Exception('Database error: '.$e->getMessage());
		}

		return $resultData;
	}


}
