<?php

namespace App\Models\InputTemplates;

use http\Exception;
use PhpOffice\PhpSpreadsheet\Spreadsheet;
use PhpOffice\PhpSpreadsheet\Writer\Xlsx;

abstract class InputTemplate
{

	const OBJECTS_LIMIT = 100000;
	const MAX_COL = 0;
	const TRAFFIC_SOURCES_MAP = [];
	const FAILED_OBJECTS_THRESHOLD_PERCENT = 10;

	/**
	 * @var Spreadsheet
	 */
	protected $spreadsheet;
	protected $csvfile;
	public $id;
	public $class;
	public $is_broadband_required;
	public $is_bandwidth_calc_required;
	public $is_lan_required;
	public $lan_config;
	public $objects;
	public $objects_validity;

	public function __construct($input, $template, $data_type)
	{
		if ($data_type === 'csv') {
			$this->csvfile = $input;
		} else {
			$this->spreadsheet = $input;
		}
		$this->id = $template->id;
		$this->class = $template->class;
		$this->is_broadband_required = $template->is_broadband_required;
		$this->is_bandwidth_calc_required = $template->is_bandwidth_calc_required;
		$this->is_lan_required = $template->is_lan_required;
	}

	public static function fingerprintCSV($filename) {
		$data = '';
		$row = 1;
		if (($handle = fopen($filename, "r")) !== FALSE) {
			while (($csvdata = fgetcsv($handle, 1000, static::detectCSVDelimiter($filename))) !== FALSE) {
				if ($row>4) break;
				$num = count($csvdata);
				$row++;
				for ($c=0; $c < $num; $c++) {
					$data .= $csvdata[$c];
				}
			}
			fclose($handle);
		}
		return sha1(trim($data));
	}

	public static function fingerprint($spreadsheet)
	{
		$ws = $spreadsheet->getActiveSheet();
		$data = '';
		foreach ($ws->getRowIterator(1, 5) as $row) {
			if ($row->getRowIndex() < 4) continue;
			foreach ($row->getCellIterator() as $cell) {
				$data .= (string)$cell;
			}
		}
		return sha1(trim($data));
	}

	/**
	 * @param $spreadsheet
	 * @param $template_data
	 * @return InputTemplate
	 * @throws \Exception
	 */
	public static function factory($data, $template_data, $data_type)
	{
		//die(var_dump('\App\Models\InputTemplates\\'.$template_data->class));
		$class_name = '\App\Models\InputTemplates\\' . $template_data->class;
		if (class_exists($class_name)) {
			return new $class_name($data, $template_data, $data_type);
		} else {
			throw new \Exception('Unknown input template class');
		}
	}

	protected function parseObjects()
	{
		$objects = [];
		$ws = $this->spreadsheet->getActiveSheet();
		$eos = false;
		$i = 0;


		foreach ($ws->getRowIterator(1, static::OBJECTS_LIMIT) as $row) {
			if ($row->getRowIndex() < 9) continue;//Skip headers
//			if (empty((string)$ws->getCellByColumnAndRow(2, $row->getRowIndex()))) {
//				//Skip if school name is empty
//				continue;
//			}
			if (empty((string)$ws->getCellByColumnAndRow(1, $row->getRowIndex())) ||
				(empty((string)$ws->getCellByColumnAndRow(2, $row->getRowIndex()))
				&& empty((string)$ws->getCellByColumnAndRow(3, $row->getRowIndex()))
				&& empty((string)$ws->getCellByColumnAndRow(4, $row->getRowIndex())))) {
				//Break if number and school name are empty
				break;
			}
			$j = 1;
			foreach ($row->getCellIterator() as $cell) {
				if ($j > static::MAX_COL) {
					break;//End of columns
				}
				$value = $cell->getOldCalculatedValue();
				if (is_null($value)) {
					$value = $cell->getCalculatedValue();
				}
				if (is_numeric($value)) {
					$value = (float) $value;
				}
				//Convert integer from possible float type to integer type to prevent issues in conditional statements
				if (is_float($value) && (int)$value==$value) {
					$value = (int) $value;
				}
				if ($j === 2 && empty($value)) {
					$value = 'School ' . ($i+1);
				}
				//echo((string)$cell->getCoordinate().': ['.$ws->getCellByColumnAndRow($j,8).']('.(string)$value.') '.(string)$cell->getValue());
				//echo("\n");
				if ($j > 8 && $value < 0) $value = 0;
				$objects[$i][$j++] = $value;
			}
			$i++;
		}

		return $objects;
	}

	protected function parseCSVObjects()
	{
		$objects = [];
		$eos = false;
		$i = 0;

		$row = 1;
		if (($handle = fopen($this->csvfile, "r")) !== FALSE) {
			while (($data = fgetcsv($handle, 10000, static::detectCSVDelimiter($this->csvfile))) !== FALSE) {
				$row++;
				if ($row < 10) continue;//Skip headers
				$num = count($data);
				//$data = array_combine(range(1, count($data)), array_values($data));
				if (empty((string)$data[1]) ||
					(empty((string)$data[2])
						&& empty((string)$data[3])
						&& empty((string)$data[4]))) {
					//Break if number and school name are empty
					break;
				}
				for ($j=0; $j < $num; $j++) {
					if ($j > static::MAX_COL) {
						break;//End of columns
					}
					$value = $data[$j];
					if (is_numeric($value)) {
						$value = (float) $value;
					}
					//Convert integer from possible float type to integer type to prevent issues in conditional statements
					if (is_float($value) && (int)$value==$value) {
						$value = (int) $value;
					}
					if ($j === 2 && empty($value)) {
						$value = 'School ' . ($i+1);
					}
					//echo((string)$cell->getCoordinate().': ['.$ws->getCellByColumnAndRow($j,8).']('.(string)$value.') '.(string)$cell->getValue());
					//echo("\n");
					if ($j > 8 && $value < 0) $value = 0;
					$objects[$i][$j+1] = $value;
				}
				$i++;
			}
			fclose($handle);
		}

		return $objects;
	}

	protected function validate_coordinates($project_settings, $object)
	{

		if ($project_settings->is_net_in_output === true) {

			$lon = $object[5];
			$lat = $object[6];

			if ((!is_numeric($lon)) || (!is_numeric($lat)))
				return false;

			if ((($lat > 90) || ($lat < -90)) || (($lon > 180) || ($lat < -180)))
				return false;
		}

		return true;
	}


	protected function validate_distance($object)
	{

		$df = $object[9];

		if (empty($df) || is_numeric($df)) {
			return true;
		}

		return false;
	}

	protected function validate_bandwidth($object)
	{

		$rb = $object[10];

		if (!is_numeric($rb))
			return false;

		if (($rb <= 0) || ($rb > 10000))
			return false;

		return true;
	}

	public function validate($project_settings, $dry_run = false)
	{
		$this->objects_validity = [];
		$failed_objects = 0;
		$errors = [];
		foreach ($this->objects as $id => $object) {
			if (($error = $this->validateObject($project_settings, $object)) !== true) {
				$errors[] = $id . ':' . $error;
				$this->objects_validity[$id] = false;
			} else {
				$this->objects_validity[$id] = true;
			}
			if (!$this->objects_validity[$id]) {
				$failed_objects++;
			}
		}
		if (!$dry_run && $failed_objects / sizeof($this->objects) * 100 >= static::FAILED_OBJECTS_THRESHOLD_PERCENT) {
			throw new \Exception('Too many invalid objects, abort calculation. ' . join(',',$errors));
		}
	}

	public function parse() {
		$this->objects = !empty($this->csvfile) ? $this->parseCSVObjects() : $this->parseObjects();
	}

	public static function detectCSVDelimiter($csvFile)
	{
		$delimiters = array(',' => 0, ';' => 0, "\t" => 0, '|' => 0, );
		$firstLine = '';
		$handle = fopen($csvFile, 'r');
		if ($handle) {
			$firstLine = fgets($handle);
			fclose($handle);
		}
		if ($firstLine) {
			foreach($delimiters as $delimiter => & $count) {
				$count = count(str_getcsv($firstLine, $delimiter));
			}
			return array_search(max($delimiters), $delimiters);
		} else {
			return key($delimiters);
		}	}

	/**
	 * Validate objects and tempalte using project settings
	 *
	 * Project settings hash:
	 * 'tp_id': number - Traffic profile id in traffic_profiles table
	 * 'is_broadband_required': true|false - Calculate broadband connection
	 * 'is_net_in_output': true|false - Calculate broadband connections between schools
	 * 'net_in_output_techs': ['cellular','satellite','rts','focl']
	 * 'net_in_output_optimization': 'npv'|'cost'
	 * 'cc': true|false - Calculate the costs and suggest suitable technologies
	 * 'is_bandwidth_calc_required': 'provided'|'calculate'|'calculate_with_tech' - Required bandwidth is provided | Calculate the required bandwidth
	 * 'is_lan_required': false|true - Calculate local area network
	 * 'lan_config':['wired':bool,'wireless':bool,'public_hotspot':bool] - Calculate local area network
	 * @param $project_settings
	 * @param $object
	 * @return mixed
	 */
	abstract protected function validateObject($project_settings, $object);
}
