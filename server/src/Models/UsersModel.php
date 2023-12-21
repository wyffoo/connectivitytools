<?php

namespace App\Models;

use PDO;

class UsersModel {

	/**
	 * @var PDO
	 */
	protected $db;

	const STATUS_INVITE_SENT = 1;
	const STATUS_INCOMING_REQUEST = 2;
	const STATUS_ASSOCIATED = 3;

	public function __construct($db) {
		$this->db = $db;
	}

	/**
	 * @param string $id
	 * @return array $user
	 * @throws Exception
	 */
	public function getUserById($id) {

		try {
			$getUserData = $this->db->prepare("SELECT * FROM users WHERE id=:id");
			$getUserData->bindParam(':id', $id, PDO::PARAM_STR);
			$getUserData->execute();

			$resultData = $getUserData->fetch(PDO::FETCH_ASSOC);

			unset($resultData['password']); // Delete password from result

		} catch (Exception $e) {
			throw new Exception($e->getMessage());
		}

		return $resultData;
	}

	/**
	 * @param string $email
	 * @return array $user
	 * @throws Exception
	 */
	public function getUserByEmail($email) {

		try {
			$getUserData = $this->db->prepare("SELECT * FROM users WHERE email=:email");
			$getUserData->bindParam(':email', $email, PDO::PARAM_STR);
			$getUserData->execute();

			$resultData = $getUserData->fetch(PDO::FETCH_ASSOC);

			unset($resultData['password']); // Delete password from result

		} catch (Exception $e) {
			throw new Exception($e->getMessage());
		}

		return $resultData;
	}



	public function updateStatus($userId, $status) {

		try {
			$update = $this->db->prepare('UPDATE users SET `status`=:status WHERE id=:user_id');
			$update->bindParam(':status', $status);
			$update->bindParam(':user_id', $userId);
			$update->execute();
		} catch (Exception $e) {
			throw new Exception($e->getMessage());
		}

		return true;
	}

	public function updateAssignedCountries($userId, $countries) {

		try {
			$this->db->beginTransaction();
			$this->db->query("DELETE FROM users_has_country WHERE users_id=".(int)$userId);

			if (sizeof($countries)>0) {
				$sql = 'INSERT INTO users_has_country (users_id,country_id) VALUES ';
				foreach ($countries as $country_id) {
					$sql .= '(' . (int)$userId . ', ' . (int)$country_id . '),';
				}
				$this->db->query(substr($sql, 0, -1) . ';');
			}

			$this->db->commit();
		} catch (Exception $e) {
			throw new Exception($e->getMessage());
		}

		return true;
	}

	/**
	 * @param string $id
	 * @return array $user
	 * @throws Exception
	 */
	public function getAllUsersList($app_mode='schools') {

		try {
			$getData = $this->db->prepare("SELECT u.id,u.email,u.firstname,u.lastname,u.role,u.last_activity_date,u.registration_date,u.status, GROUP_CONCAT(c.id) as countries
				FROM users u
				left join users_has_country uhc ON (uhc.users_id=u.id)
				left join country c ON(c.id=uhc.country_id)
				WHERE u.app_mode=:app_mode
				group by u.id");

			$getData->bindParam(':app_mode', $app_mode);
			$getData->execute();
			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);
		} catch (Exception $e) {
			throw new Exception($e->getMessage());
		}

		return $resultData;
	}

	/**
	 * @param int $userId
	 * @param int $projectId
	 * @return int $status
	 * @throws Exception
	 */
	public function isUserInProject($userId, $projectId) {

		try {
			$getUserData = $this->db->prepare("SELECT * FROM projects_has_users WHERE users_id=:users_id AND projects_id=:projects_id");
			$getUserData->bindParam(':users_id', $userId);
			$getUserData->bindParam(':projects_id', $projectId);
			$getUserData->execute();

			$resultData = $getUserData->fetch(PDO::FETCH_ASSOC);

			if(empty($resultData)) {
				return false;
			}

		} catch (Exception $e) {
			throw new Exception($e->getMessage());
		}

		return $resultData['status'];
	}

	/**
	 * @param int $userId
	 * @param int $projectId
	 * @return boolean $status
	 * @throws Exception
	 */
	public function acceptIncomingRequest($userId, $projectId) {

		try {

			$update = $this->db->prepare("UPDATE projects_has_users SET status=3 WHERE users_id=:users_id AND projects_id=:projects_id");

			// TODO: Fix error in binding status via const, temporary hard code in prepare method implemented
			//$update->bindParam(':status', self::STATUS_ASSOCIATED);
			$update->bindParam(':users_id', $userId);
			$update->bindParam(':projects_id', $projectId);
			$update->execute();

		} catch (Exception $e) {
			throw new Exception($e->getMessage());
		}

		return true;
	}

	/**
	 * @param int $userId
	 * @param int $projectId
	 * @return boolean $status
	 * @throws Exception
	 */
	public function acceptInviteByUser($userId, $projectId) {

		try {

			$update = $this->db->prepare("UPDATE projects_has_users SET status=3 WHERE users_id=:users_id AND projects_id=:projects_id");

			// TODO: Fix error in binding status via const, temporary hard code in prepare method implemented
			//$update->bindParam(':status', self::STATUS_ASSOCIATED);
			$update->bindParam(':users_id', $userId);
			$update->bindParam(':projects_id', $projectId);
			$update->execute();

		} catch (Exception $e) {
			throw new Exception($e->getMessage());
		}

		return true;
	}

	/**
	 * @param int $userId
	 * @param int $projectId
	 * @return boolean $status
	 * @throws Exception
	 */
	public function saveInvite($userId, $projectId) {

		try {

			$insert = $this->db->prepare("INSERT INTO projects_has_users ( projects_id, users_id, status ) VALUES ( ?, ?, 1 )");

			$insert->bindParam(1, $projectId);
			$insert->bindParam(2, $userId);
			// TODO: Fix error in binding status via const, temporary hard code in prepare method implemented
			//$insert->bindParam(3, self::STATUS_INVITE_SENT, PDO::PARAM_INT);

			$insert->execute();

		} catch (Exception $e) {
			throw new Exception($e->getMessage());
		}

		return true;
	}

	/**
	 * @param int $userId
	 * @param int $projectId
	 * @return boolean $status
	 * @throws Exception
	 */
	public function saveUserRequest($userId, $projectId) {

		try {

			$insert = $this->db->prepare("INSERT INTO projects_has_users ( projects_id, users_id, status ) VALUES ( ?, ?, 2 )");

			$insert->bindParam(1, $projectId);
			$insert->bindParam(2, $userId);
			// TODO: Fix error in binding status via const, temporary hard code in prepare method implemented
			//$insert->bindParam(3, self::STATUS_INVITE_SENT, PDO::PARAM_INT);

			$insert->execute();

		} catch (Exception $e) {
			throw new Exception($e->getMessage());
		}

		return true;
	}


	/**
 * @param int $projectId
 * @return array $usersList
 * @throws Exception
 */
	public function getUsersListByProjectId($projectId) {

		try {

			$getData = $this->db->prepare("SELECT users_id
				FROM projects_has_users
				WHERE projects_id=:projects_id AND `status`=:status");

			$getData->bindValue(':status', self::STATUS_ASSOCIATED);
			$getData->bindValue(':projects_id', $projectId);

			$getData->execute();

			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);

		} catch (Exception $e) {
			throw new Exception('Database error: '.$e->getMessage());
		}

		return $resultData;
	}


	/**
	 * @param UserModel $loggedInUser
	 * @return array $resultData
	 * @throws Exception
	 */
	public function getAssignedUsersList($loggedInUser) {

		try {
			$getData = $this->db->prepare("SELECT u.id,p.id as project_id, u.email,u.firstname,u.lastname,
						concat(u.firstname,' ',u.lastname) as `name`
					FROM projects_has_users pu
					JOIN projects p ON (pu.projects_id=p.id)
					JOIN users u ON (p.user_id=u.id AND u.role=:role)
					WHERE pu.users_id=:user_id AND pu.`status`=:stat");

			$getData->bindValue(':stat', self::STATUS_ASSOCIATED);
			$getData->bindValue(':role', UserModel::ROLE_ADMIN);
			$getData->bindValue(':user_id', $loggedInUser->id);

			$getData->execute();

			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);

		} catch (Exception $e) {
			throw new Exception('Database error: '.$e->getMessage());
		}

		return $resultData;
	}

	/**
	 * @param UserModel $loggedInUser
	 * @return array $requestsList
	 * @throws Exception
	 */
	public function getUsersRequests(UserModel $loggedInUser) {

		try {

			if ($loggedInUser->role === UserModel::ROLE_ADMIN) {
				$getData = $this->db->prepare("SELECT u.id,p.id as `project_id`,u.email,u.firstname,u.lastname,
						concat(u.firstname,' ',u.lastname) as `name`
					FROM projects p
					JOIN projects_has_users pu ON (p.id=pu.projects_id AND pu.status=:status)
					JOIN users u ON (pu.users_id=u.id AND u.role=:role)
					WHERE p.user_id=:user_id");
				$getData->bindValue(':status', self::STATUS_INCOMING_REQUEST);
				$getData->bindValue(':role', UserModel::ROLE_DESIGNER);
				$getData->bindValue(':user_id', $loggedInUser->id);
			} else {
				$getData = $this->db->prepare("SELECT u.id,p.id as `project_id`,u.email,u.firstname,u.lastname,
						concat(u.firstname,' ',u.lastname) as `name`
					FROM projects_has_users pu
					JOIN projects p ON (pu.projects_id=p.id)
					JOIN users u ON (p.user_id=u.id AND u.role=:role)
					WHERE pu.users_id=:user_id AND pu.`status`=:status");

				$getData->bindValue(':status', self::STATUS_INVITE_SENT);
				$getData->bindValue(':role', UserModel::ROLE_ADMIN);
				$getData->bindValue(':user_id', $loggedInUser->id);
			}

			$getData->execute();

			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);
		} catch (Exception $e) {
			throw new Exception('Database error: '.$e->getMessage());
		}

		return $resultData;
	}

	/**
	 * @param int $projectId
	 * @return array $requestsList
	 * @throws Exception
	 */
	public function getUsersRequestsToProject($projectId) {

		try {

			$getData = $this->db->prepare("SELECT users_id FROM projects_has_users WHERE projects_id=:projects_id AND status=2");

			// TODO: Fix error in binding status via const, temporary hard code in prepare method implemented
			//$getData->bindParam(':status', self::STATUS_ASSOCIATED);
			$getData->bindParam(':projects_id', $projectId);

			$getData->execute();

			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);

		} catch (Exception $e) {
			throw new Exception('Database error: '.$e->getMessage());
		}

		return $resultData;
	}

	/**
	 * @param int $userId
	 * @param int $projectId
	 * @return boolean $status
	 * @throws Exception
	 */
	public function removeUserFromProject($userId, $projectId) {

		try {

			$insert = $this->db->prepare("DELETE FROM projects_has_users WHERE users_id=:users_id AND projects_id=:projects_id");

			$insert->bindParam(':projects_id', $projectId);
			$insert->bindParam(':users_id', $userId);

			$insert->execute();

		} catch (Exception $e) {
			throw new Exception($e->getMessage());
		}

		return true;
	}

	/**
	 * @param int $userId
	 * @return boolean $status
	 * @throws Exception
	 */
	public function deleteUser($userId) {

		try {

			$delete = $this->db->prepare("DELETE FROM users WHERE id=:id");

			$delete->bindParam(':id', $userId);

			$delete->execute();

		} catch (Exception $e) {
			throw new Exception($e->getMessage());
		}

		return true;
	}

}
