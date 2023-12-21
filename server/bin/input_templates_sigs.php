<?php
require 'vendor/autoload.php';

$path = __DIR__.'/../src/Models/InputTemplates/';
$dir  = scandir($path);
array_shift($dir);
array_shift($dir);
foreach ($dir AS $fname) {
	if (strpos($fname,'xlsx')) {
		$spreadsheet = \PhpOffice\PhpSpreadsheet\IOFactory::load($path . $fname);
		$ws = $spreadsheet->getActiveSheet();
		$data = '';
		foreach ($ws->getRowIterator(1, 5) as $row) {
			if ($row->getRowIndex() < 4) continue;
			foreach ($row->getCellIterator() as $cell) {
				$data .= (string)$cell;
			}
		}
	} elseif (strpos($fname,'csv')) {
		$data = '';
		$row = 1;
		if (($handle = fopen($path . $fname, "r")) !== FALSE) {
			while (($csvdata = fgetcsv($handle, 1000, \App\Models\InputTemplates\InputTemplate::detectCSVDelimiter($path . $fname))) !== FALSE) {
				if ($row>4) break;
				$num = count($csvdata);
				//echo "$num fields in line $row:\n";
				$row++;
				for ($c=0; $c < $num; $c++) {
					$data .= $csvdata[$c];
				}
			}
			fclose($handle);
		}
		//die(var_dump($data));
	} else {
		continue;
	}
	//var_dump($data);
	echo('UPDATE giga.templates SET fingerprint="'.sha1(trim($data)).'" WHERE filename="'.$fname.'";');
	echo "\n\r";
}
