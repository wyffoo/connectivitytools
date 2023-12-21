<?php
namespace App\Models\InputTemplates;

class ExtendedBBRBLAN extends InputTemplate
{

	const MAX_COL = 18;

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
		if ($object[16] === 0 || $object[17] === 0  || ($project_settings->is_net_in_output === true && $object[18] === 0)) {
			return 'error in column 16-18';
		}

        if (!$this->validate_coordinates ($project_settings, $object))
			return 'invalid coordinates';

        if (!$this->validate_distance ($object))
			return 'invalid distance';

        if (!$this->validate_bandwidth ($object))
			return 'invalid bandwidth';

        $area_len = $object[11];

        if (!is_numeric($area_len))
			return 'invalid $area_len';

        if (($area_len<0) || ($area_len>200))
			return 'invalid $area_len range';

        $area_wid = $object[12];

        if (!is_numeric($area_wid))
			return 'invalid $area_wid';

        if (($area_wid<0) || ($area_wid>200))
			return 'invalid $area_wid range';

        $floors = $object[13];

        if (!is_numeric($floors))
			return 'invalid $floors';

        if (($floors<0) || ($floors>10))
			return 'invalid $floors range';

        $yard = $object[14];

        if (!is_numeric($yard))
            $yard = 0;

        if (($yard<0) || ($yard>10000))
			return 'invalid $yard';

        $area = $object[15];

        if (!is_numeric($area))
			return 'invalid $area';

        if (($area<0) || ($area>20000))
			return 'invalid $area range';

        return true;
	}

}
