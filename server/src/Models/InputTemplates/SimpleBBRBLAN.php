<?php
namespace App\Models\InputTemplates;

class SimpleBBRBLAN extends InputTemplate
{

	const MAX_COL = 18;
    const MAX_USERS = 50000000;

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
		//Колонки - 16 (P), 17 (Q), (18 (R) - только если выбрана галочка calculate connections between schools, если не выбрана - всё равно  что там)
		if ($object[16] === 0 || $object[17] === 0 || ($project_settings->is_net_in_output === true && $object[18] === 0)) {
			return 'error in column 16-18';
		}

        if (!$this->validate_coordinates ($project_settings, $object))
            return 'invalid coordinates';

        if (!$this->validate_distance ($object))
            return 'invalid distance';

        if (!$this->validate_bandwidth ($object))
			return 'invalid bandwidth';

        $area = $object[11];

        if (!is_numeric($area))
            $area = 0;

        if (($area<0) || ($area>static::MAX_USERS))
            return 'invalid $area';

        //Building
        for ($i = 12; $i <= 15; $i++) {
            $building = $object[$i];

            if (!is_numeric($building))
                $building = 0;

            if (($building<0) || ($building>10000))
				return 'invalid $building range';
        }

        return true;
	}

}
