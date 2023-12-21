<?php
namespace App\Models\InputTemplates;

class SimpleBBCalcLAN extends InputTemplate
{

	const MAX_COL = 39;
    const MAX_USERS = 50000000;

	const TRAFFIC_SOURCES_MAP = [
		1 => 19,
		2 => 20,
		3 => 21,
		4 => 22,
		5 => 23,
		6 => 24,
		7 => 25,
		8 => 26,
		9 => 27,
		10 => 28,
		11 => 29
	];

	protected function validateObject($project_settings, $object) {
		//Значения следующих колонок должны быть больше нуля
		//Колонки - 37 (AK), 38 (AL), (39 (AM) - только если выбрана галочка calculate connections between schools,
		if ($object[37] === 0 || $object[38] === 0 || ($project_settings->is_net_in_output === true && $object[39] === 0)) {
			return 'error in column 37-39';
		}

        if (!$this->validate_coordinates ($project_settings, $object))
            return 'invalid coordinates';

        if (!$this->validate_distance ($object))
            return 'invalid distance';

        $total_users = $object[10];

        if (!is_numeric($total_users))
			return 'invalid $total_users';

        if (($total_users<0) || ($total_users>static::MAX_USERS))
			return 'invalid $total_users range';

        $area = $object[11];

        if (!is_numeric($area))
            $area = 0;

        if (($area<0) || ($area>static::MAX_USERS))
			return 'invalid $area';

        //Devices
        for ($i = 12; $i <= 22; $i++) {
            $devices = $object[$i];

            if (!is_numeric($devices))
				return 'invalid $devices';

            if (($devices<0) || ($devices>static::MAX_USERS))
				return 'invalid $devices range';
        }

        //Building
        for ($i = 23; $i <= 27; $i++) {
            $building = $object[$i];

            if (!is_numeric($building))
                $building = 0;

            if (($building<0) || ($building>10000))
				return 'invalid $building range';
        }

        return true;
	}

}
