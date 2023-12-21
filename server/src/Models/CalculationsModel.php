<?php

namespace App\Models;

use App\Models\InputTemplates\GlobalInputTemplate;
use App\Models\InputTemplates\InputTemplate;
use PDO;
use PhpOffice\PhpSpreadsheet\Reader\Exception;
use PhpOffice\PhpSpreadsheet\Spreadsheet;
use PhpOffice\PhpSpreadsheet\Writer\Xlsx;

class CalculationsModel
{

	const ERROR_UNKNOWN_TEMPLATE = 2;

	/**
	 * @var PDO
	 */
	protected $db;

	/**
	 * @var
	 */
	protected $calculation_parameters;

	/**
	 * @var
	 */
	protected $project;

	/**
	 * CalculationsModel constructor.
	 * @param $db
	 * @param $calculation_parameters
	 */
	public function __construct($db, $calculation_parameters)
	{
		$this->db = $db;
		$this->calculation_parameters = $calculation_parameters;
	}

	/**
	 * @param $data
	 * @param false $dry_run
	 * @return InputTemplate
	 * @throws Exception
	 */
	public function loadTemplateData($data, $dry_run = false, $validation = null)
	{
		//Write data to temporary file
		//Load file
		//$input_file_name = '/mnt/Projects/giga/documents/all_examples/example_SimpleLANOnly.xlsx';
		$tmpfile = tempnam('/tmp', 'inputtdemplate');
		file_put_contents($tmpfile, $data);
		if (mime_content_type($tmpfile) === 'text/plain') {
			//CSV file
			//Find the template in the database
			if ($validation !== null) {
				$template_data = $this->findTemplateByClass($validation->template_class);
			} else {
				$fingerprint = InputTemplate::fingerprintCSV($tmpfile);
				$template_data = $this->findTemplateByFingerprint($fingerprint);
			}
			try {
				//Create appropriate input template handling class
				$template = InputTemplate::factory($tmpfile, $template_data, 'csv');
				//Parse template
				$template->parse();
				//Validate template
				$template->validate($this->calculation_parameters, $dry_run);
			} catch (\Exception $e) {
				throw new \Exception($e->getMessage());
			}
		} else {
			//XLSX
			\PhpOffice\PhpSpreadsheet\Settings::setLibXmlLoaderOptions(LIBXML_COMPACT | LIBXML_PARSEHUGE);
			$reader = \PhpOffice\PhpSpreadsheet\IOFactory::createReader('Xlsx');
			/**  Advise the Reader that we only want to load cell data  **/
			$reader->setReadDataOnly(false);
			$spreadsheet = $reader->load($tmpfile);
			//Find the template in the database
			if ($validation !== null) {
				$template_data = $this->findTemplateByClass($validation->template_class);
			} else {
				$fingerprint = InputTemplate::fingerprint($spreadsheet);
				$template_data = $this->findTemplateByFingerprint($fingerprint);
			}
			try {
				//Create appropriate input template handling class
				$template = InputTemplate::factory($spreadsheet, $template_data, 'xlsx');
				//Parse template
				$template->parse();
				//Validate template
				$template->validate($this->calculation_parameters, $dry_run);
			} catch (\Exception $e) {
				throw new \Exception($e->getMessage());
			}
		}

		return $template;
	}

	public function loadCountriesTemplateData($data, $dry_run = false, $validation = null) {
		$tmpfile = tempnam('/tmp', 'inputlocations');
		file_put_contents($tmpfile, $data);
		//CSV file
		try {
			//Create appropriate input template handling class
			$template = new GlobalInputTemplate($tmpfile);
			//Parse template
			$template->objects = $template->parseCSVLocations();
			//Validate template
			$template->validate($this->calculation_parameters, $dry_run);
		} catch (\Exception $e) {
			throw new \Exception($e->getMessage());
		}

		return $template;
	}

	/**
	 * @param array $project
	 * @param InputTemplate $template
	 * @return array
	 */
	public function prepareDataForCalculations(array $project, InputTemplate $template)
	{

		$traffic = in_array($project['mode'],['global','countries']) ? [null,null] : $this->prepareTrafficData($template);
		$varsModel = new VariablesModel($this->db);
		return [
			'shared_vars' => $varsModel->fetchVariables($project),
			'template_vars' => $varsModel->fetchTemplateVariables($template),
			'data_type' => $template->type ?? null,
			'objects' => $template->objects,
			'objects_validity' => $template->objects_validity,
			'calculation_parameters' => $this->calculation_parameters,
			'traffic_profile' => $traffic[0],
			'traffic_matrix' => $traffic[1],
			'output_template' => $this->fetchOutputTemplateInfo($template)
		];
	}

	private function prepareTrafficData($template)
	{
		$getData = $this->db->prepare("SELECT * FROM traffic_profiles WHERE id=:id");
		$getData->bindParam(':id', $this->calculation_parameters->tp_id);
		$getData->execute();
		$traffic_profile = $getData->fetch();

		if (!in_array($template->class, ['SimpleBBCalcOnly', 'ExtendedBBCalcOnly', 'SimpleBBCalcLAN', 'ExtendedBBCalcLAN'])) {
			return [$traffic_profile, []];
		}

		/**
		 *    И ещё кроме этого для случая если выбрана галочка calculate connections between schools нам понадобится значение из колонок 5 (E) и 6 (F) - но это отдельная история,
		 * там нужно будет сформировать массив опять-же (просто список школ), уже с рассчитанными Required Bandwidth, а я буду возвращать граф
		 * Т.е. для этого сначала нужно просчитать все строки
		 */

		$getData = $this->db->prepare("SELECT * FROM traffic_sources WHERE id<12");
		$getData->execute();
		$traffic_sources = $getData->fetchAll();

		$getData = $this->db->prepare("SELECT * FROM services WHERE defaultset=?");
		$traffic_profile['defaultset'] = 1;
		$getData->bindParam(1, $traffic_profile['defaultset']);
		$getData->execute();
		$services = $getData->fetchAll();

		$getData = $this->db->prepare("SELECT * FROM profile_records WHERE traffic_profiles_id=:traffic_profiles_id");
		$getData->bindParam(':traffic_profiles_id', $this->calculation_parameters->tp_id);
		$getData->execute();
		$profile_records = [];
		foreach ($getData->fetchAll() as $pf) {
			$profile_records[$pf['traffic_sources_id'] . '-' . $pf['services_id']] = $pf['using_level'];
		}

		$traffic_matrix = [];
		foreach ($template->objects as $object_id => $object) {
			$i = 0;
			foreach ($traffic_sources as $traffic_source) {
				foreach ($services as $service) {
					/**
					 * 1-я колонка - число устройств из экселя
					 */
					$traffic_matrix[$object_id][$i][0] = $object[$template::TRAFFIC_SOURCES_MAP[$traffic_source['id']]];
					/**
					 * 2-я колонка - bitrate (значение из таблицы services - колонка bitrate_low, bitrate_medium или bitrate_high выбирается в зависимости от того,
					 * что указано в колонке quality_level таблицы traffic_profiles для выбранного у тебя в системе профиля (одни и теже числа будут дублироваться для всех групп,
					 * но значения уникальные для каждого сервиса)
					 */
					$traffic_matrix[$object_id][$i][1] = floatval($service['bitrate_' . strtolower($traffic_profile['quality_level'])]);
					/**
					 * 3-я колонка - intensity (значение из таблицы services - колонка intensity_low, intensity_medium или intensity_high выбирается в зависимости от того,
					 * что указано в колонке using_level таблицы profile_records для этого сочетания номера группы (traffic_sources_id) и сервиса (services_id).
					 * ВНИМАНИЕ - если NOTUSED - то ставим значение в ноль
					 */
					$prv = strtolower($profile_records[$traffic_source['id'] . '-' . $service['id']]);
					$traffic_matrix[$object_id][$i][2] = $prv == 'notused' ? 0 : floatval($service['intensity_' . $prv]);
					/**
					 * 4-я колонка - datavolume (значение из таблицы services - колонка datavolume_low, datavolume_medium или datavolume_high выбирается в зависимости от того,
					 * что указано в колонке using_level таблицы profile_records для этого сочетания номера группы (traffic_sources_id) и сервиса (services_id).
					 * ВНИМАНИЕ - если NOTUSED - то ставим значение в ноль
					 */
					$traffic_matrix[$object_id][$i][3] = $prv == 'notused' ? 0 : floatval($service['datavolume_' . $prv]);
					$i++;
				}
			}
		}

		return [$traffic_profile, $traffic_matrix];
	}

	private function fetchOutputTemplateInfo($template)
	{
		$getData = $this->db->prepare("SELECT
       		filename,class,template_type,is_broadband_required,is_bandwidth_calc_required,is_lan_required
			FROM templates t
			WHERE t.class LIKE :class
			  AND is_broadband_required=:is_broadband_required
			  AND is_bandwidth_calc_required=:is_bandwidth_calc_required
			  AND is_lan_required=:is_lan_required
			  AND t.template_type LIKE 'OUTPUT%'");

		$is_broadband_required = !empty($this->calculation_parameters->is_broadband_required) && $this->calculation_parameters->is_broadband_required ? 1 : 0;
		$getData->bindParam(':is_broadband_required', $is_broadband_required);

		$is_bandwidth_calc_required = !empty($this->calculation_parameters->is_bandwidth_calc_required) && $this->calculation_parameters->is_bandwidth_calc_required !== 'provided' ? 1 : 0;
		$getData->bindParam(':is_bandwidth_calc_required', $is_bandwidth_calc_required);

		$is_lan_required = !empty($this->calculation_parameters->is_lan_required) && $this->calculation_parameters->is_lan_required ? 1 : 0;
		$getData->bindParam(':is_lan_required', $is_lan_required);

		$class = $template->class . '%';

		$getData->bindParam(':class', $class);
		$getData->execute();

		return $getData->fetch();
	}

	public function generateSimpleReport() {

	}

	/**
	 * @param $fingerprint
	 * @return false|mixed
	 */
	private function findTemplateByFingerprint($fingerprint)
	{
		$getData = $this->db->prepare("SELECT * FROM templates WHERE fingerprint=:fingerprint LIMIT 1");
		$getData->bindParam(':fingerprint', $fingerprint, PDO::PARAM_STR);
		$getData->execute();

		$resultData = $getData->fetchObject();

		if (empty($resultData)) {
			return false;
		}

		return $resultData;
	}

	private function findTemplateByClass($class)
	{
		$getData = $this->db->prepare("SELECT * FROM templates WHERE class=:class LIMIT 1");
		$getData->bindParam(':class', $class, PDO::PARAM_STR);
		$getData->execute();

		$resultData = $getData->fetchObject();

		if (empty($resultData)) {
			return false;
		}

		return $resultData;
	}

	static public function who_is_the_winner_by_costofownership($case, $focl, $microvawe, $satellite, $cellular)
	{
		switch ($case) {
			case 0:
				return '';
				break;
			case 1:
				return 'Cellular';
				break;
			case 2:
				return 'Satellite';
				break;
			case 3:
				if ($satellite > $cellular)
					return 'Cellular';
				else
					return 'Satellite';
				break;
			case 4:
				return 'Microwave';
				break;
			case 5:
				if ($microvawe > $cellular)
					return 'Cellular';
				else
					return 'Microwave';
				break;
			case 6:
				if ($microvawe > $satellite)
					return 'Satellite';
				else
					return 'Microwave';
				break;
			case 7:
				if ($microvawe > $satellite) {
					if ($satellite > $cellular)
						return 'Cellular';
					else
						return 'Satellite';
				} else {
					if ($microvawe > $cellular)
						return 'Cellular';
					else
						return 'Microwave';
				}
				break;
			case 8:
				return 'Fiber';
				break;
			case 9:
				if ($focl > $cellular)
					return 'Cellular';
				else
					return 'Fiber';
				break;
			case 10:
				if ($focl > $satellite)
					return 'Satellite';
				else
					return 'Fiber';
				break;
			case 11:
				if ($focl > $satellite) {
					if ($satellite > $cellular)
						return 'Cellular';
					else
						return 'Satellite';
				} else {
					if ($focl > $cellular)
						return 'Cellular';
					else
						return 'Fiber';
				}
				break;
			case 12:
				if ($focl > $microvawe)
					return 'Microwave';
				else
					return 'Fiber';

				break;
			case 13:
				if ($focl > $microvawe) {
					if ($microvawe > $cellular)
						return 'Cellular';
					else
						return 'Microwave';
				} else {
					if ($focl > $cellular)
						return 'Cellular';
					else
						return 'Fiber';
				}
				break;
			case 14:
				if ($focl > $microvawe) {
					if ($microvawe > $satellite)
						return 'Satellite';
					else
						return 'Microwave';
				} else {
					if ($focl > $satellite)
						return 'Satellite';
					else
						return 'Fiber';
				}
				break;
			case 15:
				if ($focl > $microvawe) {
					if ($microvawe > $satellite) {
						if ($satellite > $cellular)
							return 'Cellular';
						else
							return 'Satellite';
					} else {
						if ($microvawe > $cellular)
							return 'Cellular';
						else
							return 'Microwave';
					}
				} else {
					if ($focl > $satellite) {
						if ($satellite > $cellular)
							return 'Cellular';
						else
							return 'Satellite';
					} else {
						if ($focl > $cellular)
							return 'Cellular';
						else
							return 'Fiber';
					}
				}
				break;
			default:
				return '';
				break;
		}
		return '';
	}


	static public function who_is_the_winner_by_npv($case, $focl, $microvawe, $satellite, $cellular)
	{
		switch ($case) {
			case 0:
				return '';
				break;
			case 1:
				return 'Cellular';
				break;
			case 2:
				return 'Satellite';
				break;
			case 3:
				if ($satellite < $cellular)
					return 'Cellular';
				else
					return 'Satellite';
				break;
			case 4:
				return 'Microwave';
				break;
			case 5:
				if ($microvawe < $cellular)
					return 'Cellular';
				else
					return 'Microwave';
				break;
			case 6:
				if ($microvawe < $satellite)
					return 'Satellite';
				else
					return 'Microwave';
				break;
			case 7:
				if ($microvawe < $satellite) {
					if ($satellite < $cellular)
						return 'Cellular';
					else
						return 'Satellite';
				} else {
					if ($microvawe < $cellular)
						return 'Cellular';
					else
						return 'Microwave';
				}
				break;
			case 8:
				return 'Fiber';
				break;
			case 9:
				if ($focl < $cellular)
					return 'Cellular';
				else
					return 'Fiber';
				break;
			case 10:
				if ($focl < $satellite)
					return 'Satellite';
				else
					return 'Fiber';
				break;
			case 11:
				if ($focl < $satellite) {
					if ($satellite < $cellular)
						return 'Cellular';
					else
						return 'Satellite';
				} else {
					if ($focl < $cellular)
						return 'Cellular';
					else
						return 'Fiber';
				}
				break;
			case 12:
				if ($focl < $microvawe)
					return 'Microwave';
				else
					return 'Fiber';

				break;
			case 13:
				if ($focl < $microvawe) {
					if ($microvawe < $cellular)
						return 'Cellular';
					else
						return 'Microwave';
				} else {
					if ($focl < $cellular)
						return 'Cellular';
					else
						return 'Fiber';
				}
				break;
			case 14:
				if ($focl < $microvawe) {
					if ($microvawe < $satellite)
						return 'Satellite';
					else
						return 'Microwave';
				} else {
					if ($focl < $satellite)
						return 'Satellite';
					else
						return 'Fiber';
				}
				break;
			case 15:
				if ($focl < $microvawe) {
					if ($microvawe < $satellite) {
						if ($satellite < $cellular)
							return 'Cellular';
						else
							return 'Satellite';
					} else {
						if ($microvawe < $cellular)
							return 'Cellular';
						else
							return 'Microwave';
					}
				} else {
					if ($focl < $satellite) {
						if ($satellite < $cellular)
							return 'Cellular';
						else
							return 'Satellite';
					} else {
						if ($focl < $cellular)
							return 'Cellular';
						else
							return 'Fiber';
					}
				}
				break;
			default:
				return '';
				break;
		}
		return '';
	}


}
