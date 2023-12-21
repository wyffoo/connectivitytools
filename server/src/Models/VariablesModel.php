<?php

namespace App\Models;

use PDO;

class VariablesModel {

	protected $container;

	/**
	 * @var PDO
	 */
	protected $db;

	public function __construct($db) {
		$this->db = $db;
	}

	public function resetVariables($project) {
		$getData = $this->db->prepare("DELETE FROM projects_has_variables WHERE projects_id=?");
		$getData->bindValue(1,$project['id']);
		$getData->execute();
		return true;
	}

	public function updateProjectVars($project, $vars) {
		$this->db->beginTransaction();
		foreach ($vars AS $var) {
			$getData = $this->db->prepare("INSERT INTO projects_has_variables (projects_id,variables_id,value) VALUES (?,?,?)
				ON DUPLICATE KEY UPDATE value=?");
			$getData->bindValue(1,$project['id']);
			$getData->bindValue(2,$var[0]);
			$getData->bindValue(3,$var[1]);
			$getData->bindValue(4,$var[1]);
			$getData->execute();
		}
		$this->db->commit();
	}

	public function fetchMainVariables() {
		//Main variables
		$getData = $this->db->prepare("SELECT * FROM variables ORDER BY var_class DESC, technology DESC");
		$getData->execute();
		$variables = $getData->fetchAll();

		return $variables;
	}

	public function fetchVariables($project) {

		$merge = function ($target, $source, $field) {
			foreach ($source AS $source_item) {
				foreach ($target AS &$target_item) {
					if ($target_item[$field] === $source_item[$field]) {
						$target_item = $source_item;
					}
				}
			}
			return $target;
		};

		//Main variables
		if ($project['mode'] === 'global') {
			$getData = $this->db->prepare("SELECT * FROM variables WHERE app_mode = 'all' OR app_mode in ('global','countries') ORDER BY var_class DESC, technology DESC");
		} else {
			$getData = $this->db->prepare("SELECT * FROM variables WHERE app_mode = 'all' OR app_mode = :app_mode ORDER BY var_class DESC, technology DESC");
			$getData->bindParam(':app_mode', $project['mode']);
		}
		$getData->execute();
		$variables = $getData->fetchAll();

		//Region variables
		$getData = $this->db->prepare("SELECT v.*,TRIM(rhv.value) AS value FROM variables v JOIN region_has_variables rhv
					ON (rhv.variables_id=v.id AND rhv.region_id=(SELECT region_id FROM country c2 WHERE c2.id=:country_id)) ORDER BY v.var_class DESC, v.technology DESC");
		$getData->bindParam(':country_id', $project['country_id']);
		$getData->execute();
		$region_variables = $getData->fetchAll();
		$variables = $merge($variables, $region_variables, 'id');

		//Country variables
		$getData = $this->db->prepare("SELECT v.*,TRIM(chv.value) AS value FROM variables v JOIN country_has_variables chv ON (chv.variables_id=v.id AND chv.country_id=:country_id) ORDER BY v.var_class DESC, v.technology DESC");
		$getData->bindParam(':country_id', $project['country_id']);
		$getData->execute();
		$country_variables = $getData->fetchAll();
		$variables = $merge($variables, $country_variables, 'id');

		//Project variables
		$getData = $this->db->prepare("SELECT v.*,TRIM(phv.value) AS value,1 AS is_custom FROM variables v JOIN projects_has_variables phv ON (phv.variables_id=v.id AND phv.projects_id=:project_id) ORDER BY v.var_class DESC, v.technology DESC");
		$getData->bindParam(':project_id', $project['id']);
		$getData->execute();
		$project_variables = $getData->fetchAll();
		$variables = $merge($variables, $project_variables, 'id');

		return $variables;
	}

	public function fetchTemplateVariables ($template) {
		//Template variables
		$getData = $this->db->prepare("SELECT v.*,thv.templates_col_number FROM variables v JOIN templates_has_variables thv ON (thv.variables_id=v.id AND thv.templates_id=:template_id)");
		$getData->bindParam(':template_id', $template->id);
		$getData->execute();
		return $getData->fetchAll();
	}

}
