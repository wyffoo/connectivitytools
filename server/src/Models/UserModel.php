<?php

namespace App\Models;

class UserModel {

	public $id;
	public $email;
	public $firstname;
	public $lastname;
	public $contrycode;
	public $role;
	public $preferred_language;
	public $app_mode;

	const ROLE_SYSADMIN = 'sysadmin';
	const ROLE_ADMIN = 'admin';

	public function __construct($data) {
		$this->id = $data['id'];
		$this->email = $data['email'];
		$this->firstname = $data['firstname'];
		$this->lastname = $data['lastname'];
		$this->role = $data['role'];
		$this->preferred_language = $data['preferred_language'];
		$this->app_mode = $data['app_mode'];
	}

	public function toArray() {
		return [
			'id'    => $this->id,
			'email' => $this->email,
			'firstname' => $this->firstname,
			'lastname' => $this->lastname,
			'contrycode' => $this->contrycode,
			'role' => $this->role,
			'preferred_language' => $this->preferred_language,
			'app_mode' => $this->app_mode
		];
	}
}
