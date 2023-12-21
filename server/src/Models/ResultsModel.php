<?php

namespace App\Models;

use PDO;

class ResultsModel {

	/**
	 * @var PDO
	 */
	protected $db;

	/**
	 * UserController constructor.
	 * @param \Slim\Container $container
	 */
	public function __construct($db) {
		$this->db = $db;
	}


	/**
	 * @param int $objectId
	 * @return array $results
	 * @throws Exception
	 */
	public function getResultsByProjectId($type, $project_id, $run_id = null) {

		$project_id = (int)$project_id;
		try {
			$sql = "SELECT object_id AS id,data,created FROM calculation_results WHERE project_id = {$project_id} AND type=:type AND run_id=";
			if ($run_id === null) {
				$sql .= "(SELECT run_id FROM calculation_results WHERE project_id = {$project_id} ORDER BY id DESC LIMIT 1)";
			} else {
				$sql .= ":run_id";
			}
//			$sql .= " ORDER BY object_id ASC";

			$getData = $this->db->prepare($sql);
			$getData->bindParam(':type', $type);
			if ($run_id !== null) {
				$getData->bindParam(':run_id', $run_id);
			}
			//die(var_dump($getData->debugDumpParams()));
			$getData->execute();
			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);
		} catch (Exception $e) {
			throw new Exception('Database error: '.$e->getMessage());
		}

		return $resultData;
	}

	public function getReportsByProjectId($type, $project_id, $run_id = null) {

		$project_id = (int)$project_id;
		try {
			$sql = "SELECT data,created FROM calculation_reports WHERE project_id = {$project_id} AND type=:type AND run_id=";
			if ($run_id === null) {
				$sql .= "(SELECT run_id FROM calculation_results WHERE project_id = {$project_id} ORDER BY id DESC LIMIT 1)";
			} else {
				$sql .= ":run_id";
			}

			$getData = $this->db->prepare($sql);
			$getData->bindParam(':type', $type);
			if ($run_id !== null) {
				$getData->bindParam(':run_id', $run_id);
			}
			//die(var_dump($getData->debugDumpParams()));
			$getData->execute();
			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);
		} catch (Exception $e) {
			throw new Exception('Database error: '.$e->getMessage());
		}

		return $resultData;
	}


	public function getLogsByProjectId($type, $project_id, $run_id = null) {

		$project_id = (int)$project_id;
		try {
			$sql = "SELECT object_id AS id,data,created FROM calculation_logs WHERE project_id = {$project_id} AND type=:type AND run_id=";
			if ($run_id === null) {
				$sql .= "(SELECT run_id FROM calculation_results WHERE project_id = {$project_id} ORDER BY id DESC LIMIT 1)";
			} else {
				$sql .= ":run_id";
			}
			$sql .= " ORDER BY object_id ASC";

			$getData = $this->db->prepare($sql);
			$getData->bindParam(':type', $type);
			if ($run_id !== null) {
				$getData->bindParam(':run_id', $run_id);
			}
			//die(var_dump($getData->debugDumpParams()));
			$getData->execute();
			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);
		} catch (Exception $e) {
			throw new Exception('Database error: '.$e->getMessage());
		}

		return $resultData;
	}

	/**
	 * @param integer $objectId
	 * @return array
	 * @throws Exception
	 */
	public function getResultsProtocolByObjectId($objectId) {
		try {
			$q = $this->db->prepare("SELECT t.name as technology_name, r.npv, r.protocol
				FROM results r
				JOIN technologies t ON (t.id=r.technologies_id)
				WHERE r.object_id=:object_id
				ORDER BY r.npv DESC");

			$q->bindValue(':object_id', $objectId);

			$q->execute();

			return $q->fetchAll();
		} catch (Exception $e) {
			throw new Exception('Database error: '.$e->getMessage());
		}
	}

	public static function compressData($data) {
		return COMPRESS_PROJECT_BLOBS ? base64_encode(@gzcompress($data, 9)): $data;
	}

	public static function decompressData($data) {
		return COMPRESS_PROJECT_BLOBS && preg_match('%^[a-zA-Z0-9/+]*={0,2}$%', $data) ? @gzuncompress(base64_decode($data)) : $data;
	}

	public static function stripFilenameCharacters($str) {
		$str = preg_replace("/[^a-zA-Z0-9\s]/", '', $str);
		$str = preg_replace("([\. ]{2,})", '', $str);
		return $str;
	}
}
