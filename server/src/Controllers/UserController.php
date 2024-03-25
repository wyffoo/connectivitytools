<?php

namespace App\Controllers;

use App\Models\ProjectModel;
use App\Models\UserModel;
use App\Models\UsersModel;
use PDO;
use Psr\Http\Message\ResponseInterface as Response;
use ReCaptcha\ReCaptcha;
use Aws\Lambda\LambdaClient;
use Exception;
class UserController extends Controller {

	/**
	 * @return Response
	 */
	public function action_authenticatedAPI() {
		if ($this->request->getAttribute('user')) {
			$user = $this->request->getAttribute('user');
			return $this->respondWithData($user);
		} else {
			return $this->respondWithData('');
		}
	}

	/**
	 * @return Response
	 */
	public function action_verifyEmailIsRegisteredAPI() {
		$data = $this->request->getParsedBody();
		if (empty($data['email'])) {
			return $this->response->withStatus(400);
		}
	
		// Determine app_mode from request or domain
		$app_mode = 'global'; // Default value
		if (isset($data['app_mode'])) {
			$app_mode = $data['app_mode'];
		} else {
			if (strpos($_SERVER['HTTP_HOST'], 'countries') !== false) {
				$app_mode = 'countries';
			} elseif (strpos($_SERVER['HTTP_HOST'], 'schools') !== false) {
				$app_mode = 'schools';
			}
			// Add more conditions as needed
		}
	
		$users = $this->getUserByEmail($data['email']);
		foreach ($users as $user) {
			if ($user['app_mode'] === $app_mode) {
				return $this->respondWithData(['status' => 'taken']);
			}
		}
	
		return $this->respondWithData(['status' => 'available']);
	}
	

	/**
	 * @return Response
	 */
	public function action_setLanguage() {
		$data = $this->request->getParsedBody();
		$user = $this->request->getAttribute('user');

		if (!$user) {
			return $this->response->withStatus(400);
		}

		if (empty($data['lang']) || !in_array($data['lang'], array('en','ru','ua'))) {
			return $this->response->withStatus(500);
		}

		$this->updateUserLanguage($user->email, $data['lang']);

		return $this->respondWithData(['status' => 'success']);
	}

	public function action_updateUserCountriesAPI() {
		$user = $this->request->getAttribute('user');

		if (!$user || $user->role !== UserModel::ROLE_SYSADMIN) {
			$this->deny();
		}

		$data = $this->request->getParsedBody();
		$usersModel = new UsersModel($this->db);
		$usersModel->updateAssignedCountries($data['user_id'],$data['countries']);

		return $this->respondWithData(['status' => 'success']);
	}

	/**
	 * @return Response
	 */
	public function action_signInAPI() {
		$data = $this->request->getParsedBody();

		if (empty($data['email']) || empty($data['password'])) {
			return $this->response->withStatus(400);
		}

		if (isset($data['isCountriesMode'])  && $data['isCountriesMode'] === true) {
			$app_mode = 'countries';
		} else if (isset($data['isSchoolsMode'])  && $data['isSchoolsMode'] === true) {
			$app_mode = 'schools';
		} else {
			$app_mode = 'global';
		}

		try {
			$this->login($data['email'], $data['password'], false, $app_mode);
			return $this->respondWithData(['token' => session_id()]);
		} catch (\Exception $e) {
			return $this->respondWithData($e->getMessage(), 500);
		}
	}

	/**
	 * @return mixed
	 */
	public function action_signOutAPI() {
		session_destroy();
		return $this->respondWithData(['data' => "success"]);
	}
	
	/**
	 * @return Response
	 */
	public function action_signUpAPI() {
		$data = $this->request->getParsedBody();

		if (empty($data['email'])
			|| empty($data['password'])
			|| empty($data['firstname'])
			|| empty($data['lastname'])
			|| empty($data['recaptchaToken'])
			|| ($data['code'] !== 'giga2021' && $data['code'] !== 'countries2021' && $data['code'] !== 'global2023')
		) {
			return $this->response->withStatus(400, "Wrong parameters");
		}

		try {
			$data['app_mode'] = 'global';
			if (strpos($_SERVER['HTTP_HOST'],'countries') !== false) {
				$data['app_mode'] = 'countries';
			}
			if (strpos($_SERVER['HTTP_HOST'],'schools') !== false) {
				$data['app_mode'] = 'schools';
			}
			if (strpos($_SERVER['HTTP_HOST'],'global') !== false) {
				$data['app_mode'] = 'global';
			}
			return $this->respondWithData(['data' => $this->signUp($data)]);
		} catch (\Exception $e) {
			return $this->response->withStatus(500, $e->getMessage());
		}
	}

	/**
	 * @return Response
	 */
	public function action_passwordResetAPI() {
		session_start();
		$data = $this->request->getParsedBody();
	
		$action = $data['action'] ?? '';
	
		switch ($action) {
			case 'sendCode':
				return $this->sendVerificationCode($data);
			case 'verifyCode':
				return $this->verifyCodeAndUpdatePassword($data);
			default:
				return $this->response->withStatus(400, "Invalid action");
		}
	}
	
	private function sendVerificationCode($data) {
		if (empty($data['email'])) {
			return $this->response->withStatus(400, "Email is required");
		}
	
		if (!$this->validateUserAlreadyRegistered($data['email'])) {
			return $this->response->withStatus(400, "Email is not registered");
		}
	
		$verificationCode = rand(100000, 999999);
		$_SESSION['verificationCode'] = $verificationCode;
		$_SESSION['emailForVerification'] = $data['email'];
	
		// Sending the verification code via an AWS Lambda function
		$ch = curl_init('https://r6vdavnnld.execute-api.eu-central-1.amazonaws.com/beta2');
		$payload = json_encode([
			'email' => $data['email'],
			'verificationcode' => $verificationCode,
		]);
	
		curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "POST");
		curl_setopt($ch, CURLOPT_POSTFIELDS, $payload);
		curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
		curl_setopt($ch, CURLOPT_HTTPHEADER, [
			'Content-Type: application/json',
			'Content-Length: ' . strlen($payload)
		]);
	
		$result = curl_exec($ch);
		$httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
		curl_close($ch);
	
		if ($httpCode == 200) {
			return $this->respondWithData(['message' => 'Verification code sent. Please check your email.']);
		} else {
			return $this->response->withStatus(500, "Failed to send verification code");
		}
	}
	
	private function verifyCodeAndUpdatePassword($data) {
		if ($_SESSION['verificationCode'] == $data['code'] && $_SESSION['emailForVerification'] == $data['email']) {
			if ($this->updatePassword($data['email'], $data['newPassword'])) {
				return $this->respondWithData(['message' => 'Password has been reset successfully.']);
			} else {
				return $this->response->withStatus(500, "Failed to reset password");
			}
		} else {
			return $this->response->withStatus(400, "Invalid verification code");
		}
	}
	

	public function action_passwordUpdateAPI() {
		$user = $this->authorize();

		if (!$user->id) {
			$this->deny();
		}

		$data = $this->request->getParsedBody();

		if (empty($data['newPassword']) || empty($data['oldPassword'])) {
			return $this->response->withStatus(400, "Wrong parameters");
		}

		if (!$this->validatePassword($data['oldPassword'])) {
			return $this->response->withStatus(400, "Wrong parameters");
		}

		try {
			if ($this->login($user->email, $data['oldPassword'], true)) {
				$this->updatePassword($user->email, $data['newPassword']);
			}
		} catch (\Exception $e) {
			return $this->response->withStatus(400, "Wrong parameters");
			//throw new \Exception($e->getMessage(), 500);
		}

		return $this->respondWithData(['status' => "success"]);
	}

	/**
	 * @return mixed
	 */
	public function action_getAllUsersAPI() {
		if ($this->request->getAttribute('user')) {
			$user = $this->request->getAttribute('user');

			if($user->role === UserModel::ROLE_SYSADMIN) {

				$usersModel = new UsersModel($this->db);
				$users = $usersModel->getAllUsersList($user->app_mode);
				foreach ($users AS &$user) {
					if (!empty($user['countries'])) {
						$user['assigned_countries'] = explode(',',$user['countries']);
					}
				}
				return $this->respondWithData($users);
			} else {
				$this->deny();
			}
		} else {
			$this->deny();
		}
	}

	public function action_updateUserStatusAPI() {
		if ($this->request->getAttribute('user')) {
			$user = $this->request->getAttribute('user');

			if($user->role === UserModel::ROLE_SYSADMIN) {

				$data = $this->request->getParsedBody();
				$usersModel = new UsersModel($this->db);
				if (!in_array($data['status'],['active','suspended','deleted'])) {
					$this->deny();
				}
				$usersModel->updateStatus($data['user_id'], $data['status']);

				return $this->respondWithData(['status' => "success"]);

			} else {
				$this->deny();
			}

		} else {
			$this->deny();
		}
	}

	/**
	 * @return mixed
	 */
	public function action_deleteUserAPI() {
		if ($this->request->getAttribute('user')) {
			$user = $this->request->getAttribute('user');

			if($user->role === UserModel::ROLE_SYSADMIN) {

				$usersModel = new UsersModel($this->container);
				$usersModel->deleteUser($this->args['id']);

				return $this->respondWithData(['status' => "success"]);

			} else {

				throw new \Exception("Access denied.", 500);
			}

		} else {

			throw new \Exception("Access denied.", 500);
		}
	}

	/**
	 * @param string $email
	 * @param string $password
	 * @return mixed|null
	 * @throws Exception
	 */
	private function login($email, $password, $dry_run = false, $app_mode = 'schools') {
		$email = strtolower($email);

		// validate email and password
		if ($this->validateEmail($email) == false) {
			throw new \Exception("Sorry, this email is invalid.", 200);
		}
		if ($this->validatePassword($password) == false) {
			throw new \Exception("Sorry, this password is invalid.", 200);
		}

		try {

			$resultData = null;

			//Get current password hash
			$getLoginData = $this->db->prepare("SELECT
				id, email, `password`, firstname, lastname, role, preferred_language, app_mode
				FROM users WHERE email=:email AND status='active' AND app_mode=:app_mode");
			$getLoginData->bindParam(':email', $email, PDO::PARAM_STR);
			$getLoginData->bindParam(':app_mode', $app_mode);
			$getLoginData->execute();

			$resultData = $getLoginData->fetch(PDO::FETCH_ASSOC);

			if ($resultData == false) {
				throw new \Exception("Login fail!");
			}

			if (password_verify($password . $email, $resultData['password']) === true) {
				unset($resultData['password']);
			} else {
				throw new \Exception('Invalid password');
			}

		} catch (\Exception $e) {
			throw new \Exception("Login fail!");
		}

		if (isset($resultData['email']) && isset($resultData['role'])) {
			if (!$dry_run) {
				$_SESSION['authenticated_user'] = new UserModel($resultData);
			}
			return true;
		} else {
			throw new \Exception("Login fail!");
		}
	}

	/**
	 * @param $data
	 * @return mixed|null
	 * @throws Exception
	 */
	private function signUp($data) {

		$data['email'] = strtolower($data['email']);

		// validate recaptcha
		if ($this->validateRecaptcha($data['recaptchaToken']) === false) {
			throw new Exception("Sorry, bots are not allowed.", 500);
		}

		// validate username, password and email
		if ($this->validatePassword($data['password']) === false) {
			throw new Exception("Sorry, this password is invalid.", 500);
		}
		if ($this->validateEmail($data['email']) === false) {
			throw new Exception("Sorry, this email is invalid.", 500);
		}

		if ($this->validateUserAlreadyRegistered($data['email']) === true) {
			throw new Exception("Sorry, this username/email is already registered.", 500);
		}

		$encrypted_password = password_hash($data['password'] . $data['email'], PASSWORD_BCRYPT);

		try {

			$insert = $this->db->prepare("INSERT INTO users
				( email, `password`, firstname, lastname, role, app_mode, registration_date ) VALUES ( ?, ?, ?, ?, ?, ?, UTC_TIMESTAMP())");

			$data['role'] = UserModel::ROLE_ADMIN;

			$insert->bindParam(1, $data['email']);
			$insert->bindParam(2, $encrypted_password);
			$insert->bindParam(3, $data['firstname']);
			$insert->bindParam(4, $data['lastname']);
			//$insert->bindParam(5, $data['countrycode']);
			$insert->bindParam(5, $data['role']);
			$insert->bindParam(6, $data['app_mode']);

			$insert->execute();

		} catch (\Exception $e) {
			throw new \Exception('Database error');
		}

		// login through of right way
		return true;
	}

	private function getUserByEmail($email) {
		try {
			$getUserData = $this->db->prepare("SELECT * FROM users WHERE email=:email");
			$getUserData->bindParam(':email', $email, PDO::PARAM_STR);
			$getUserData->execute();
	
			$resultData = $getUserData->fetchAll(PDO::FETCH_ASSOC);
	
			return $resultData; 
		} catch (Exception $e) {
			throw new Exception($e->getMessage());
		}
	}
	

	/**
	 * @param string $email
	 * @return bool
	 * @throws Exception
	 */
	private function validateUserAlreadyRegistered($email) {
		$app_mode = 'global'; // Default value
			if (strpos($_SERVER['HTTP_HOST'], 'countries') !== false) {
				$app_mode = 'countries';
			} elseif (strpos($_SERVER['HTTP_HOST'], 'schools') !== false) {
				$app_mode = 'schools';
			}
			try {
				// add a condition to check app_mode
				$query = "SELECT email FROM users WHERE email = :email AND app_mode = :app_mode";
				$getUserData = $this->db->prepare($query);
				$getUserData->bindParam(':email', $email, PDO::PARAM_STR);
				$getUserData->bindParam(':app_mode', $app_mode, PDO::PARAM_STR);
				$getUserData->execute();
		
				$resultData = $getUserData->fetch(PDO::FETCH_ASSOC);
		
				return !empty($resultData); 
			} catch (Exception $e) {
				throw new Exception($e->getMessage());
			}
	}

	private function updateUserPassword($email, $hashedPassword) {
		try {
			$query = "UPDATE users SET password = :password WHERE email = :email";
			$stmt = $this->db->prepare($query);
			$stmt->bindParam(':email', $email, PDO::PARAM_STR);
			$stmt->bindParam(':password', $hashedPassword, PDO::PARAM_STR);
			return $stmt->execute();
		} catch (\PDOException $e) {
			throw new Exception("Database error: " . $e->getMessage());
		}
	}

	/**
	 * @param $email
	 * @param $lang
	 * @return bool
	 * @throws Exception
	 */
	function updateUserLanguage($email, $lang) {
		try {
			$update = $this->db->prepare("UPDATE users SET preferred_language=:lang WHERE email=:email");
			$update->bindParam(':email', $email, PDO::PARAM_STR);
			$update->bindParam(':lang', $lang, PDO::PARAM_STR);
			$update->execute();

			$_SESSION['authenticated_user']->preferred_language = $lang;

			return true;

		} catch (Exception $e) {
			throw new Exception($e->getMessage());
		}
	}

	/**
	 * @param $password
	 * @return bool
	 */
	private function validatePassword($password) {

		// must contains one digit from 0-9
		// must contains one lowercase characters
		// must contains one uppercase characters
		// match anything with previous condition checking length at least 8 characters and maximum of 20
		$passwordRules = "((?=.*\d)(?=.*[a-z])(?=.*[A-Z]).{8,32})";

		if (!preg_match($passwordRules, $password)) {
			return false;
		}

		return true;

	}

	/**
	 * @param $email
	 * @return bool
	 */
	private function validateEmail($email) {

		/* @todo need be improved. */
		$emailRule = "(^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$)";

		if (!preg_match($emailRule, $email)) {
			return false;
		}

		return true;

	}

	/**
	 * @param $token
	 * @return bool
	 */
	private function validateRecaptcha($token) {
		$settings = $this->container->get('settings')['recaptcha'];
		$recaptcha = new ReCaptcha($settings['secret'], new \ReCaptcha\RequestMethod\CurlPost());
		$resp = $recaptcha->verify($token);
		if ($resp->isSuccess()) {
			return true;
		} else {
			//$errors = $resp->getErrorCodes();
			return false;
		}
	}

	/**
	 * @param string $email
	 * @param string $password
	 * @return mixed|null
	 * @throws Exception
	 */
	private function updatePassword($email, $password) {
		if ($this->validateUserAlreadyRegistered($email) === false) {
			throw new Exception("Sorry, this email is not registered.", 500);
		}
		if($this->validatePassword($password) === false) {
			throw new Exception("Sorry, this password is invalid.", 500);
		}
		$encrypted_password = password_hash($password . $email, PASSWORD_BCRYPT);

		try {
			$update = $this->db->prepare("UPDATE users SET `password` = :pass WHERE email = :email");
			$update->bindParam(':email', $email, PDO::PARAM_STR);
			$update->bindParam(':pass', $encrypted_password, PDO::PARAM_STR);
			$update->execute();

			return true;

		} catch (Exception $e) {
			throw new Exception($e->getMessage());
		}
	}

}
