<?php
namespace App\Models\OutputTemplates;
use App\Models\CalculationsModel;

class SimpleLANOnly extends OutputTemplate
{
    protected $formulas = [
        12 => '=IF(OR(ISBLANK(J99),ISBLANK(K99)),"",IF(AND(ISNUMBER(J99),ISNUMBER(K99)),IF(AND(J99>=0,K99>=0),J99+K99*5,""),""))',
        13 => '=IF(OR(ISBLANK(J99),ISBLANK(K99)),"",IF(AND(ISNUMBER(J99),ISNUMBER(K99)),IF(AND(J99>=0,K99>=0),J99+K99*10,""),""))',
        14 => '=IF(OR(ISBLANK(J99),ISBLANK(K99)),"",IF(AND(ISNUMBER(J99),ISNUMBER(K99)),IF(AND(J99>=0,K99>=0),J99+K99*15,""),""))',
        15 => '=IF(OR(ISBLANK(J99),ISBLANK(K99)),"",IF(AND(ISNUMBER(J99),ISNUMBER(K99)),IF(AND(J99>=0,K99>=0),J99+K99*20,""),""))'
    ];

	public function process_simple($objects, $calculation_parameters, $results) {
		//Call the main process to fill base cells
		$this->process($objects, $calculation_parameters, $results);
	}

    public function process_smart($objects, $calculation_parameters, $results) {
        //Call the main process to fill base cells
        $this->process($objects, $calculation_parameters, $results, true);
    }


    public function process($objects, $calculation_parameters, $results, $fillformulas = false) {
		parent::process($objects, $calculation_parameters, $results);
		$sheet = $this->spreadsheet->getSheet(0);
		$results_objects = $results['objects'];
		$row_index = 7;//Start row
		$object_index = 0;
		foreach ($objects AS $object) {
			$col_index = 1;
			foreach ($object as $col) {
				//Колонки 1 (A) - 9 (I) просто сохраняем в память и "как есть" (один к одному) перегоняем в теже колонки в выходном темплейте SimpleLANOnly.xlsx
				if ($col_index >= 1 && $col_index <= 9) {
					$sheet->setCellValueByColumnAndRow($col_index, $row_index, $col);
				}
				//Override with calculated DTF
				if ($col_index === 9 && empty($col) && !empty($object_results->calculated_dtf)) {
					$sheet->setCellValueByColumnAndRow($col_index, $row_index, $object_results->calculated_dtf);
				}
				$col_index++;
			}
			//Колонка 10 (J)  сюда положи CAPEX
			//Колонка 11 (K)  положи OPEX
			$object_results = json_decode($results_objects[$object_index]['data']);
			$main_results = $object_results->main;
			list($capex, $opex)  = $this->calculateCAPEXOPEX($object_results->costs, $calculation_parameters);
			$sheet->setCellValueByColumnAndRow(10, $row_index, $capex);
			$sheet->setCellValueByColumnAndRow(11, $row_index, $opex);

            if ($fillformulas)
            {
                $this->fillFormulas($sheet, $row_index);
            }
            else
            {
                $lan_capex = (float) $capex;
                $lan_opex = (float) $opex;

                if (($lan_capex > 0) && ($lan_opex > 0))
                {
                    $sheet->setCellValueByColumnAndRow(12, $row_index, $lan_capex + 5*$lan_opex);
                    $sheet->setCellValueByColumnAndRow(13, $row_index, $lan_capex + 10*$lan_opex);
                    $sheet->setCellValueByColumnAndRow(14, $row_index, $lan_capex + 15*$lan_opex);
                    $sheet->setCellValueByColumnAndRow(15, $row_index, $lan_capex + 20*$lan_opex);
                }
            }

			$object_index++;
			$row_index++;
		}
	}

}
