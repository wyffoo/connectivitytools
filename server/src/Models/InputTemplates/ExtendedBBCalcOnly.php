<?php
namespace App\Models\InputTemplates;

class ExtendedBBCalcOnly extends InputTemplate
{

	const MAX_COL = 31;
	const MAX_USERS = 50000000;

	const TRAFFIC_SOURCES_MAP = [
		1 => 17,
		2 => 18,
		3 => 19,
		4 => 20,
		5 => 21,
		6 => 22,
		7 => 23,
		8 => 24,
		9 => 25,
		10 => 26,
		11 => 27
	];

	protected function validateObject($project_settings, $object) {
		//Значения следующих колонок должны быть больше нуля
		//Колонки - 30 (AD), (31 (AE) - только если выбрана галочка calculate connections between schools, если не выбрана - всё равно что там)
		if ($object[30] === 0 || ($project_settings->is_net_in_output === true && $object[31] === 0)) {
			return 'error in column 30-31';
		}

        if (!$this->validate_coordinates ($project_settings, $object))
			return 'invalid coordinates';

        if (!$this->validate_distance ($object))
			return 'invalid distance';

        $pupils_pr = $object[10];

        if (!is_numeric($pupils_pr))
			return 'invalid $pupils_pr';

        if (($pupils_pr<0) || ($pupils_pr>static::MAX_USERS))
            return 'invalid $pupils_pr range';

        $pupils_sec = $object[11];

        if (!is_numeric($pupils_sec))
            return 'invalid $pupils_sec';

        if (($pupils_sec<0) || ($pupils_sec>static::MAX_USERS))
			return 'invalid $pupils_sec range';

        $pupils_high = $object[12];

        if (!is_numeric($pupils_high))
			return 'invalid $pupils_high';

        if (($pupils_high<0) || ($pupils_high>static::MAX_USERS))
            return 'invalid $pupils_high range';

        $teachers = $object[13];

        if (!is_numeric($teachers))
            return 'invalid $teachers';

        if (($teachers<0) || ($teachers>static::MAX_USERS))
            return 'invalid $teachers range';

        $adm = $object[14];

        if (!is_numeric($adm))
            $adm = 0;

        if (($adm<0) || ($adm>static::MAX_USERS))
            return 'invalid $adm';

        $guests = $object[15];

        if (!is_numeric($guests))
            $guests = 0;

        if (($guests<0) || ($guests>static::MAX_USERS))
            return 'invalid $guests';


        $total = $object[16];

        if (!is_numeric($total))
            return 'invalid $total';

        if (($total<0) || ($total>static::MAX_USERS))
            return 'invalid $total range';


        //Devices
        for ($i = 17; $i <= 27; $i++) {
            $devices = $object[$i];

            if (!is_numeric($devices))
                return 'invalid $devices';

            if (($devices<0) || ($devices>static::MAX_USERS))
                return 'invalid $devices range';
        }

        return true;
	}

}
