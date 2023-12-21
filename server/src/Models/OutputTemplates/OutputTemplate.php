<?php
namespace App\Models\OutputTemplates;

use App\Models\VariableModel;
use Matrix\Exception;
use PhpOffice\PhpSpreadsheet\Comment;
use PhpOffice\PhpSpreadsheet\Spreadsheet;
use PhpOffice\PhpSpreadsheet\Writer\Xlsx;
use App\Models\CalculationsModel;

abstract class OutputTemplate
{

	/**
	 * @var Spreadsheet
	 */
	protected $spreadsheet;

	protected $formulas;

    public function __construct($input_file_name)
	{
		$spreadsheet = \PhpOffice\PhpSpreadsheet\IOFactory::load($input_file_name);
		$this->spreadsheet = $spreadsheet;
	}

	protected function setFormula(\PhpOffice\PhpSpreadsheet\Worksheet\Worksheet $sheet, $col, $row) {
		try {
			$sheet->setCellValueByColumnAndRow($col, $row, str_replace('99', $row, $this->formulas[$col]));
		} catch (\Exception $e) {
			$sheet->setCellValueByColumnAndRow($col, $row, '-ERROR-');
		}
	}

	protected function fillFormulas($sheet, $row_index) {
		if (empty($this->formulas)) return true;
		foreach ($this->formulas AS $col=>$value) {
			$this->setFormula($sheet, $col, $row_index);
		}
	}

	public function save($file_name) {
		setlocale(LC_ALL, 'en_US');
		$this->spreadsheet->setActiveSheetIndex(0);
		$writer = new Xlsx($this->spreadsheet);
		$writer->save($file_name);
		unset($this->spreadsheet);
	}

	protected function calculateCAPEXOPEX($result, $settings) {
		//  Грубо говоря 1,3,5,7 это CAPEX
		//	2,4,6,8 - OPEX
		//
		//  Но когда мы только выбираем что нам считать в LAN у нас есть картинка
		//	Там три галочки
		// Так вот
		//	в CAPEX полюбому складываем значение 1
		//	в OPEX полюбому складываем значение 2
		//
		// Далее по галочкам - если первая галочка выбрана была (не знаю где ты это хранишь) - Wired network in classrooms.
		//			То в CAPEX добавляем к тому что там есть значение из 3
		//а в OPEX добавляем к тому что там есть значение из 4
		//
		// Если вторая выбрана  - Wireless network in classrooms*.
		//То в CAPEX добавляем к тому что там есть значение из 5
		//а в OPEX добавляем к тому что там есть значение из 6
		//
		//
		//Если третья выбрана - Public wireless hotspot** area*.
		//То в CAPEX добавляем к тому что там есть значение из 7
		//а в OPEX добавляем к тому что там есть значение из 8
		$capex = 0;
		$opex = 0;
		$capex += $result[0];
		$opex += $result[1];
		//'lan_config': ['no','wired','wireless','public_hotspot'] - Calculate local area network
		if ($settings->lan_config->wired === true) {
			$capex += $result[2];
			$opex += $result[3];
		} else if ($settings->lan_config->wireless === true) {
			$capex += $result[4];
			$opex += $result[5];
		} else if ($settings->lan_config->public_hotspot === true) {
			$capex += $result[6];
			$opex += $result[7];
		}
		return [round($capex,2), round($opex,2)];
	}

	/**
	 * @param $objects
	 * @param $results
	 * @param $calculation_parameters
	 * @return mixed
	 */
	public function process($objects, $calculation_parameters, $results) {
		//Attach a second sheet if is_net_in_output is true and insert topology matrix there
		if ($calculation_parameters->is_net_in_output === true && !empty($results['topology'])) {
			$sheet = $this->spreadsheet->getSheet(1);
			$topology = json_decode($results['topology']['data']);
			foreach ($topology->main AS $row_index=>$row) {
				if (!is_array($row)) break;
				$row_name = $objects[$row_index]->{2};
				$sheet->setCellValueByColumnAndRow(1,$row_index+2,$row_name);
				foreach ($row AS $col_index=>$col) {
					if ($row_index === 0) {
						$col_name = $objects[$col_index]->{2};
						$sheet->setCellValueByColumnAndRow($col_index+2,1,$col_name);
					}

					$sheet->setCellValueByColumnAndRow($col_index+2,$row_index+2,empty($col) ? '-' : $col);

//					if ($results->topology->log[$row_index][$col_index] === '-') continue;
//					$sheet->getCommentByColumnAndRow($col_index + 2, $row_index + 2)->setWidth("400px")
//						->getText()->createTextRun($results->topology->log[$row_index][$col_index]);
				}
			}
		} else {
			//Delete second worksheet
			//$this->spreadsheet->removeSheetByIndex(1);
		}
	}

}
