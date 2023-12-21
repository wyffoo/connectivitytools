<?php

namespace App\Models;

class VariableModel {

	/**
	 * @var \Slim\Container
	 */
	protected $container;

	/**
	 * @var PDO
	 */
	protected $db;

	/**
	 * UserController constructor.
	 * @param \Slim\Container $container
	 */
	public function __construct(\Slim\Container $container) {
		$this->container = $container;
		$this->db = $container->get('db');
	}


	/**
	 * @param string $name
	 * @return array $variable
	 */
	public function getVariableByName($name) {
		try {

			$getData = $this->db->prepare("SELECT * FROM variables WHERE name=:name");

			$getData->bindParam(':name', $name);

			$getData->execute();

			$resultData = $getData->fetch(PDO::FETCH_ASSOC);

		} catch (Exception $e) {
			throw new Exception('Database error');
		}

		return $resultData;
	}

	/**
	 * @param int $id
	 * @return array $variable
	 */
	public function getVariableById($id) {
		try {

			$getData = $this->db->prepare("SELECT * FROM variables WHERE id=:id");

			$getData->bindParam(':id', $id);

			$getData->execute();

			$resultData = $getData->fetch(PDO::FETCH_ASSOC);

		} catch (Exception $e) {
			throw new Exception('Database error');
		}

		return $resultData;
	}

	/**
	 * @param string $class
	 * @return array $variables
	 */
	public function getRequiredVariables($class) {
		try {

			$getData = $this->db->prepare("SELECT * FROM variables WHERE class=:class AND required=1");

			$getData->bindParam(':class', $class);

			$getData->execute();

			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);

		} catch (Exception $e) {
			throw new Exception('Database error');
		}

		return $resultData;
	}

	/**
	 * @param string $class
	 * @return array $variables
	 */
	public function getVariablesListByClass($class, $lang = 'ru') {
		try {

			if (in_array($lang, array('en','ru','ua')) && $lang != 'ru') {
				$select = "id,name,type,default_value,required,class,tech_level,
				description_{$lang} as description,options_{$lang} as options,unit_{$lang} as unit";
			} else {
				$select = 'id,name,description,type,default_value,unit,required,class,tech_level,options';
			}

			$getData = $this->db->prepare("SELECT {$select} FROM variables WHERE class=:class");

			$getData->bindParam(':class', $class);

			$getData->execute();

			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);

		} catch (Exception $e) {
			throw new Exception('Database error');
		}

		return $resultData;
	}

	/**
	 * @param int $id
	 * @return array $variables
	 */
	public function getVariablesListByProjectId($id) {
		try {

			$getData = $this->db->prepare("SELECT variables_id,value FROM projects_has_variables WHERE projects_id=:projects_id");

			$getData->bindParam(':projects_id', $id);

			$getData->execute();

			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);

		} catch (Exception $e) {
			throw new Exception('Database error');
		}

		return $resultData;
	}

	/**
	 * @param int $id
	 * @return array $variables
	 */
	public function getVariablesListByObjectId($id) {
		try {

			$getData = $this->db->prepare("SELECT variables_id,value FROM objects_has_variables WHERE objects_id=:objects_id");

			$getData->bindParam(':objects_id', $id);

			$getData->execute();

			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);

		} catch (Exception $e) {
			throw new Exception('Database error');
		}

		return $resultData;
	}

	/**
	 * @param int $id
	 * @return array $variables
	 */
	public function getVariablesListByTechnologyId($id) {
		try {

			$getData = $this->db->prepare("SELECT variables_id,default_value FROM technologies_has_variables WHERE technologies_id=:technologies_id");

			$getData->bindParam(':technologies_id', $id);

			$getData->execute();

			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);

		} catch (Exception $e) {
			throw new Exception('Database error');
		}

		return $resultData;
	}

	/**
	 * @param int $tech_id
	 * @param int $project_id
	 * @return array $variables
	 */
	public function getVariablesListByTechnologyIdAndProjectId($tech_id, $project_id) {
		try {

			$getData = $this->db->prepare("SELECT technologies_has_variables.variables_id,technologies_has_variables.default_value FROM projects_has_technologies LEFT JOIN technologies_has_variables ON (projects_has_technologies.technologies_id=technologies_has_variables.technologies_id) WHERE projects_has_technologies.projects_id=:project_id AND projects_has_technologies.technologies_id=:technologies_id");

			$getData->bindParam(':project_id', $project_id);
			$getData->bindParam(':technologies_id', $tech_id);

			$getData->execute();

			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);

		} catch (Exception $e) {
			throw new Exception('Database error');
		}

		return $resultData;
	}

	/**
	 * @return array $variables
	 */
	public function getVariablesCompatibility() {
		try {

			$getData = $this->db->prepare("SELECT * FROM variables_has_variables WHERE projects_id IS null");

			$getData->execute();

			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);

		} catch (Exception $e) {
			throw new Exception('Database error');
		}

		return $resultData;
	}

	/**
	 * @param int $id
	 * @return array $variables
	 */
	public function getVariablesCompatibilityForProject($projectId) {
		try {

			$getData = $this->db->prepare("SELECT * FROM variables_has_variables WHERE projects_id=:projects_id");

			$getData->bindParam(':projects_id', $projectId);

			$getData->execute();

			$resultData = $getData->fetchAll(PDO::FETCH_ASSOC);

		} catch (Exception $e) {
			throw new Exception('Database error');
		}

		return $resultData;
	}

}
