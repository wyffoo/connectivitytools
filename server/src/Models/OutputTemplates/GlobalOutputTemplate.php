<?php

namespace App\Models\OutputTemplates;

use http\Exception;
use PDO;
use PhpOffice\PhpSpreadsheet\Spreadsheet;
use PhpOffice\PhpSpreadsheet\Writer\Xlsx;

class GlobalOutputTemplate
{
	public function __construct($query)
	{
		$this->query = $query;
	}


	public function generateLocationsCSV($filename, $type)
	{
		switch ($type) {
			case 'city':
				$this->generateLocationsCitiesCSV($filename);
				break;
			case 'social_point':
				$this->generateLocationsSocialPointsCSV($filename);
				break;
			default://hexagons
				$this->generateLocationsHexagonsCSV($filename);
				break;
		}
	}

	private function generateLocationsCitiesCSV($filename) {
		$fp = fopen($filename, 'w');
		fputcsv($fp, ['identifier','name','alt_name','lat','long','admin_name','admin_code','population','flag','area']);
		while ($row = $this->query->fetch(PDO::FETCH_NUM)) {
			for($i=0;$i<8;$i++) {
				$row[$i]=trim($row[$i]);
			}
			fputcsv($fp, $row);
		}
		fclose($fp);
	}

	private function generateLocationsSocialPointsCSV($filename) {
		$fp = fopen($filename, 'w');
		fputcsv($fp, ['identifier','name','alt_name','lat','long','admin_name','admin_code','population','flag','area']);
		while ($row = $this->query->fetch(PDO::FETCH_NUM)) {
			for($i=0;$i<8;$i++) {
				$row[$i]=trim($row[$i]);
			}
			fputcsv($fp, $row);
		}
		fclose($fp);
	}

	private function generateLocationsHexagonsCSV($filename) {
		/**
		hex_id -> locations.identifier (example "867a6a8afffffff") //Cannot be empty
		lat -> locations.lat                //Cannot be empty
		lng -> locations.long               //Cannot be empty
		pops -> locations.population        //Cannot be empty, but could be 0
		size -> locations.type              //Cannot be empty
			h6 ->  'hexagon_6'
			h7 ->  'hexagon_7'
			h8 ->  'hexagon_8'
			h9 ->  'hexagon_9'
		parent -> locations.parent_id       //Can be empty
		rb -> locations.rb                  //Can be empty
		flag -> locations.flag              //Can be empty
			so -> 'so'         //Coordinates of social object
			cn -> 'cn'         //Connected node
		 */
		$fp = fopen($filename, 'w');
		fputcsv($fp, ['hex_id','lat','lng','pops','size','parent','rb','flag']);
		while ($row = $this->query->fetch(PDO::FETCH_NUM)) {
			for($i=0;$i<8;$i++) {
				$row[$i]=trim($row[$i]);
			}
			$row[4] = str_replace('exagon_','',$row[4]);
			fputcsv($fp, $row);
		}
		fclose($fp);

	}
	protected function validateObject($project_settings, $object) {}

	public function process() {}
}
