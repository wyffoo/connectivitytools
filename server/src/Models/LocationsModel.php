<?php

namespace App\Models;

use PDO;

class LocationsModel {

	protected $container;

	protected $db;

	public function __construct($db) {
		$this->db = $db;
	}

	public function deleteLocation($profile) {
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

	public function getAvailableCountries() {
		try {

			$query = $this->db->prepare("SELECT * from country WHERE is_verified=1 OR id IN (SELECT DISTINCT(country_id) FROM locations)");
			$query->execute();
			$data = $query->fetchAll(PDO::FETCH_ASSOC);
		} catch (\Exception $e) {
			throw new \Exception('Database error: '.$e->getMessage());
		}

		return $data;
	}

	public function getLocations($country_id, $type, $limit = 15, $offset = 0, $order_by = '', $search = []) {
		if (!$country_id) {
			return ['total'=>0,'data'=>[]];
		}

		if (!empty($order_by) && $order_by[0] === '-') {
			$desc = true;
			$field = substr($order_by,1);
		} else {
			$desc = false;
			$field = $order_by;
		}
		if (in_array($field,['population','identifier','name'])) {
			$order = 'ORDER BY ' . $field . ($desc ? ' DESC' : ' ASC');
		} else {
			$order = '';
		}
		try {
			$limit = (int)$limit;
			$offset = (int)$offset;
			$query = $this->db->prepare("SELECT COUNT(*) FROM locations WHERE `country_id`=:country AND `type`=:type");
			$query->bindParam(1, $country_id);
			$query->bindParam(2, $type);
			$query->execute();
			$total = $query->fetchColumn(0);
			$query = $this->db->prepare("SELECT * FROM locations WHERE `country_id`=:country AND `type`=:type " . $order . " LIMIT {$offset}, {$limit}");
			$query->bindParam(1, $country_id);
			$query->bindParam(2, $type);
			//die(var_dump($query));
			$query->execute();
			$data = $query->fetchAll(PDO::FETCH_ASSOC);
		} catch (\Exception $e) {
			throw new \Exception('Database error: '.$e->getMessage());
		}

		return ['total'=>$total,'data'=>$data];
	}

	public function getLocation($user_id, $profile_id) {
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

	public function updateLocation($profile_id, $profile) {
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

	public function addLocations($profile) {
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



	public function exportLocations($filename, $country_id, $type) {
		try {
			$type = in_array($type,['city','social_point','hexagon_6','hexagon_7','hexagon_8','hexagon_9']) ? $type : 'city';
			if ($type === 'city') {
				$fields = '`identifier`,`name`,`alt_name`,`lat`,`long`,`admin_name`,`admin_code`,`population`,`flag`,`area`';
			} elseif ($type == 'social_point') {
				$fields = '`identifier`,`name`,`lat`,`long`,`admin_name`,`admin_code`,`population`,`flag`,`area`';
			} else {
				$fields = '`identifier`,`lat`,`long`,`population`,`type`,`parent_identifier`,`rb`,`flag`';
			}

			$query = $this->db->prepare("SELECT {$fields} FROM locations WHERE country_id = ? AND `type` = ?");
			$query->bindParam(1, $country_id);
			$query->bindParam(2, $type);
			$query->execute();

			return $query;
		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}
	}

	public function importLocations($filename, $country_id, $type) {
		try {
			$country_id = (int)$country_id;
			$type = in_array($type, ['city', 'social_point', 'hexagon_6', 'hexagon_7', 'hexagon_8', 'hexagon_9']) ? $type : 'city';
			$this->db->setAttribute(PDO::ATTR_EMULATE_PREPARES, true);
			$this->db->setAttribute(PDO::MYSQL_ATTR_USE_BUFFERED_QUERY, true);
	
			if ($type === 'city') {
				// The fields for 'city' type
				$fields = '`identifier`, `name`, `alt_name`, `lat`, `long`, `admin_name`, `admin_code`, `population`, `flag`, `area`';
			} elseif ($type == 'social_point') {
				// The fields for 'social_point' type
				$fields = '`identifier`, `name`, `alt_name`, `lat`, `long`, `admin_name`, `admin_code`, `population`, `flag`, `area`';
			} else {
				// The fields for other types
				$fields = '`identifier`, `lat`, `long`, `population`, `area`, `parent_identifier`, `rb`, `flag`';
			}
	
			$query = $this->db->prepare("LOAD DATA LOW_PRIORITY LOCAL INFILE '{$filename}'
					INTO TABLE locations
					CHARACTER SET utf8
					FIELDS TERMINATED BY ';' OPTIONALLY ENCLOSED BY '\"' ESCAPED BY '\"'
					IGNORE 1 LINES ({$fields}) SET `type`='{$type}', `country_id`='{$country_id}'");
			$query->execute();
		} catch (\Exception $e) {
			throw new \Exception('Database error: ' . $e->getMessage());
		}
	}
	


}