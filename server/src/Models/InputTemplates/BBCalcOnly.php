<?php
namespace App\Models\InputTemplates;

class BBCalcOnly extends InputTemplate
{

	const MAX_COL = 12;

	const TRAFFIC_SOURCES_MAP = [
		1 => 18,
		2 => 19,
		3 => 20,
		4 => 21,
		5 => 22,
		6 => 23,
		7 => 24,
		8 => 25,
		9 => 26,
		10 => 27,
		11 => 28
	];

	protected function validateObject($project_settings, $object) {
		//Значения следующих колонок должны быть больше нуля
		//олонки - 11 (K), (12 (L)  - только если выбрана галочка calculate connections between schools, если не выбрана - всё равно  что там)
		if ($object[11] === 0 || ($project_settings->is_net_in_output === true && $object[12] === 0)) {
			return 'error in column 11-12';
		}

        if (!$this->validate_coordinates ($project_settings, $object))
            return 'invalid coordinates';

		if (!$this->validate_distance ($object))
            return 'invalid distance';



        return true;
	}

}
