<?php

namespace App\Models\InputTemplates;

use http\Exception;
use PhpOffice\PhpSpreadsheet\Spreadsheet;
use PhpOffice\PhpSpreadsheet\Writer\Xlsx;

class GlobalInputTemplate extends InputTemplate
{
	public $csvfile;
	public $locations;
	public $type = '';

	public function __construct($filename)
	{
		$this->csvfile = $filename;
	}

	public function parseCSVLocations()
	{
		$locations = [];
		$eos = false;
		$i = 0;

		$row = 1;
		if (($handle = fopen($this->csvfile, "r")) !== FALSE) {
			while (($data = fgetcsv($handle, 10000, static::detectCSVDelimiter($this->csvfile))) !== FALSE) {
				$row++;
				if ($row === 2) {
					//Detect type of CSV data
					if (strtolower($this->remove_utf8_bom(trim($data[0]))) === 'hex_id') {//hexagon
						$this->type = 'hexagon';
					} elseif (isset($data[0]) && strtolower(trim($data[0])) === 'objectname' && isset($data[7]) && strtolower(trim($data[7])) === 'poc'){
						$this->type = 'global_ext';
					} elseif (isset($data[0]) && strtolower(trim($data[0])) === 'objectname'){
						$this->type = 'global';
					} elseif (strtolower(trim($data[2])) === 'alt_name') {//city
						$this->type = 'city';
					} else {
						$this->type = 'social_point';
					}
				}
				if ($row < 3) continue;//Skip headers
				$num = sizeof($data);
				//$data = array_combine(range(1, count($data)), array_values($data));
				for ($j=0; $j < $num; $j++) {
					$value = $data[$j];
					if (is_numeric($value)) {
						$value = (float) $value;
					}
					//Convert integer from possible float type to integer type to prevent issues in conditional statements
					if (is_float($value) && (int)$value==$value) {
						$value = (int) $value;
					}
					//echo("\n");
					//if ($j > 8 && $value < 0) $value = 0;
					$locations[$i][$j+1] = trim($value);
				}
				$i++;
			}
			fclose($handle);
		}

		return $locations;
	}

	public function parseCSVObjects() {
		return $this->parseCSVLocations();
	}

	protected function validateObject($project_settings, $object) {
		return true;
	}

	private function remove_utf8_bom($text) {
		$bom = pack('H*','EFBBBF');
		$text = preg_replace("/^$bom/", '', $text);
		return $text;
	}
}
