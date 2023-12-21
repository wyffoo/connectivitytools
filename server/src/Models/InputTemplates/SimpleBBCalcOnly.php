<?php

namespace App\Models\InputTemplates;

class SimpleBBCalcOnly extends InputTemplate
{

	const MAX_COL = 32;
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

	protected function validateObject($project_settings, $object)
	{
		//Значения следующих колонок должны быть больше нуля
		//Колонки - 31 (AD), (32 (AE) - только если выбрана галочка calculate connections between schools, если не выбрана - всё равно что там)
		if ($object[31] === 0 || ($project_settings->is_net_in_output === true && $object[32] === 0)) {
			return 'error in columns 31-32';
		}

		if (!$this->validate_coordinates($project_settings, $object))
			return 'invalid coordinates';

		if (!$this->validate_distance($object))
			return 'invalid distance';

        $total_users = $object[10];

        if (!is_numeric($total_users))
            return 'invalid $total_users';

        if (($total_users<0) || ($total_users>static::MAX_USERS))
			return 'invalid $total_users range';

        //Devices
        for ($i = 11; $i <= 21; $i++) {
            $devices = $object[$i];

            if (!is_numeric($devices))
				return 'invalid $devices';

            if (($devices<0) || ($devices>static::MAX_USERS))
				return 'invalid $devices range';
        }

		return true;
	}

}
