<?php
namespace App\Models\InputTemplates;

class SimpleLANOnly extends InputTemplate
{

	const MAX_COL = 10;

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
		//Колонки - 10 (J)
		if ($object[10] === 0) {
			return 'error in column 10';
		}

        $area = $object[5];

        if (!is_numeric($area))
            $area = 0;

        if (($area<0) || ($area>5000))
            return 'invalid $area';

        //Building
        for ($i = 6; $i <= 9; $i++) {
            $building = $object[$i];

            if (!is_numeric($building))
                $building = 0;

            if (($building<0) || ($building>10000))
				return 'invalid $building range';
        }

		return true;
	}


}
