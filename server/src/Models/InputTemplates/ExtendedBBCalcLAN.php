<?php
namespace App\Models\InputTemplates;

class ExtendedBBCalcLAN extends InputTemplate
{

	const MAX_COL = 37;
    const MAX_USERS = 50000000;

	const TRAFFIC_SOURCES_MAP = [
		1 => 22,
		2 => 23,
		3 => 24,
		4 => 25,
		5 => 26,
		6 => 27,
		7 => 28,
		8 => 29,
		9 => 30,
		10 => 31,
		11 => 32
	];

	protected function validateObject($project_settings, $object) {
		//Значения следующих колонок должны быть больше нуля
		//Колонки - 35 (AI), 36 (AJ), (37 (AK) - только если выбрана галочка calculate connections between schools, если не выбрана - всё равно  что там)
		//- тут и в других случаях будет вопрос И или ИЛИ.
		//Пока давай поставим И (в смысле если хотя бы одна из двух ячеек меньше 1, то не считаем вообще эту строку)
		if ($object[35] === 0 || $object[36] === 0 || ($project_settings->is_net_in_output === true && $object[37] === 0)) {
			return 'error in column 35-37';
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
            return 'invalid $guests range';


        $total = $object[16];

        if (!is_numeric($total))
            return 'invalid $total';

        if (($total<0) || ($total>static::MAX_USERS))
            return false;

        $area_len = $object[17];

        if (!is_numeric($area_len))
            return 'invalid $area_len';

        if (($area_len<0) || ($area_len>200))
            return 'invalid $area_len range';

        $area_wid = $object[18];

        if (!is_numeric($area_wid))
            return 'invalid $area_wid';

        if (($area_wid<0) || ($area_wid>200))
            return 'invalid $area_wid range';

        $floors = $object[19];

        if (!is_numeric($floors))
            return 'invalid $floors';

        if (($floors<0) || ($floors>10))
            return 'invalid $floors range';

        $yard = $object[20];

        if (!is_numeric($yard))
            $yard = 0;

        if (($yard<0) || ($yard>10000))
            return 'invalid $yard range';

        $area = $object[21];

        if (!is_numeric($area))
            return 'invalid $area';

        if (($area<0) || ($area>20000))
            return 'invalid $area range';


        //Devices
        for ($i = 22; $i <= 32; $i++) {
            $devices = $object[$i];

            if (!is_numeric($devices))
                return 'invalid $devices';

            if (($devices<0) || ($devices>static::MAX_USERS))
                return 'invalid $devices range';
        }

        return true;
	}

}
