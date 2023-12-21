<?php

namespace App\Controllers;

use App\Models\CalculationsModel;
use App\Models\ProjectModel;
use App\Models\ResultsModel;
use App\Models\UserModel;
use GeoJson\Feature\Feature;
use GeoJson\Feature\FeatureCollection;
use GeoJson\Geometry\LineString;
use GeoJson\Geometry\MultiPoint;
use GeoJson\Geometry\Point;
use Predis\Command\GeospatialGeoPos;
use Slim\Psr7\Stream;

class ResultsController extends Controller
{

	public function action_projectTopologySummary()
	{
		set_time_limit(120);
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id'], true);

		if ((in_array($user->app_mode,['global','countries']) && $user->role !== UserModel::ROLE_SYSADMIN) && $project['user_id'] !== $user->id) {
			$this->deny();
		}

		$resultsModel = new ResultsModel($this->db);
		$topology = $resultsModel->getResultsByProjectId('topology', $project['id']);

		if (empty($topology)) {
			return $this->respondWithData(null);
		}

		$topology = json_decode(array_pop($topology)['data']);

		if (isset($topology->main)) {//topo version 1
			$summary = $topology->summary;
		} else {//topo version 2
			$summary = [];
			//"summary": [["Total_NPV", "-358", "USD"],
			// ["Total_number_of_links", "2", "links"],
			// ["Number_of_links_FOCL", "2 (100%)", "links"],
			// ["Number_of_links_Microwave", "0 (0%)", "links"],
			// ["Number_of_links_Satellite", "0 (0%)", "links"],
			// ["Number_of_links_Cellular", "0 (0%)", "links"],
			// ["NPV_of_links_FOCL", "-358 (100%)", "USD"],
			// ["NPV_of_links_Microwave", "0 (0%)", "USD"],
			// ["NPV_of_links_Satellite", "0 (0%)", "USD"],
			// ["NPV_of_links_Cellular", "0 (0%)", "USD"]]}
			$mapping = [//translation key, unit
				'Total links' => ['Total_number_of_links','links'],
				'FOCL' => ['Number_of_links_FOCL', 'links'],
				'RTS' => ['Number_of_links_Microwave', 'links'],
				'SAT' => ['Number_of_links_Satellite', 'links'],
				'MOB' => ['Number_of_links_Cellular', 'links'],
				'NPV Total' => ['NPV_total', 'USD'],
				'NPV cumulative total' => ['NPV_cumulative_total', 'USD'],
				'NPV cumulative addon' => ['NPV_cumulative_addon', 'USD'],
				'Number of Clusters' => ['Number_of_Clusters','clusters']
			];

			if ($user->app_mode === 'global') {
				$lines = array_combine(array_values($topology->summary[0]->Name),array_values($topology->summary[0]->Value));
				foreach ($lines as $name=>$value) {
					$summary[] = [$mapping[$name][0] ?? $name, $value, $mapping[$name][1] ?? 'units'];
				}
			} else {
				foreach ($topology->summary[0] as $line) {
					$summary[] = [$mapping[$line->Name][0] ?? $line->Name, $line->Value, $mapping[$line->Name][1] ?? 'units'];
				}
			}

		}

		//Process saved data
		if (!empty($summary)) {
			return $this->respondWithData($summary);
		}

		return $this->respondWithData(null);
	}

	public function action_projectSummary()
	{
		set_time_limit(120);
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id'], true);

		if ((in_array($user->app_mode,['global','countries']) && $user->role !== UserModel::ROLE_SYSADMIN) && $project['user_id'] !== $user->id) {
			$this->deny();
		}

		$resultsModel = new ResultsModel($this->db);
		$objects = $resultsModel->getResultsByProjectId('object', $project['id']);

		//Process saved data
		if (!empty($project['inputdata'])) {
			$input = json_decode($project['inputdata']);
			if (empty($input->objects)) {
				return $this->respondWithData([]);
			}
		} else {
			return $this->respondWithData([]);
		}

		if (!empty($project['validationdata'])) {
			$validation = json_decode($project['validationdata']);
		}

		$input_objects = $input->objects;
		$diagrams = [];

		if (empty($validation)) {
			return $this->respondWithData([]);
		}

		if (empty($validation->valid_objects) && empty($validation->total_objects)) {//Old hack for existing projects
			$validation->total_objects = sizeof($objects);
			$validation->valid_objects = $validation->total_objects - sizeof($validation->invalid_objects);
		}
		$summary = [
			['name' => 'total_objects', 'value' => isset($validation->total_objects) ? $validation->total_objects : 'NaN', 'units' => in_array($user->app_mode,['global','countries']) ? 'value_objects' : 'value_schools'],
			['name' => 'valid_objects', 'value' => isset($validation->valid_objects) ? $validation->valid_objects : 'NaN', 'units' => in_array($user->app_mode,['global','countries']) ? 'value_objects' : 'value_schools'],
			['name' => 'invalid_objects', 'value' => sizeof($validation->invalid_objects), 'units' => 'value_schools']
		];
		$total_required_bandwidth = 0;
		$mean_required_bandwidth = 0;
		$total_capex_for_all_recommended = 0;
		$mean_capex_for_all_recommended = 0;
		$total_opex_for_all_recommended = 0;
		$mean_opex_for_all_recommended = 0;

		$total_costofownership5_for_all_recommended = 0;
		$mean_costofownership5_for_all_recommended = 0;

		$total_costofownership10_for_all_recommended = 0;
		$mean_costofownership10_for_all_recommended = 0;
		$total_costofownership15_for_all_recommended = 0;
		$mean_costofownership15_for_all_recommended = 0;
		$total_costofownership20_for_all_recommended = 0;
		$mean_costofownership20_for_all_recommended = 0;
		$total_npv5_for_all_recommended = 0;
		$mean_npv5_for_all_recommended = 0;
		$total_npv10_for_all_recommended = 0;
		$mean_npv10_for_all_recommended = 0;
		$total_npv15_for_all_recommended = 0;
		$mean_npv15_for_all_recommended = 0;
		$total_npv20_for_all_recommended = 0;
		$mean_npv20_for_all_recommended = 0;

		//NEW RESULTS
		$total_capex_focl_all_recommended_costofownership5 = 0;
		$total_capex_mw_all_recommended_costofownership5 = 0;
		$total_capex_sat_all_recommended_costofownership5 = 0;
		$total_capex_cel_all_recommended_costofownership5 = 0;

		$total_capex_focl_all_recommended_costofownership20 = 0;
		$total_capex_mw_all_recommended_costofownership20 = 0;
		$total_capex_sat_all_recommended_costofownership20 = 0;
		$total_capex_cel_all_recommended_costofownership20 = 0;

		$total_opex_focl_all_recommended_costofownership5 = 0;
		$total_opex_mw_all_recommended_costofownership5 = 0;
		$total_opex_sat_all_recommended_costofownership5 = 0;
		$total_opex_cel_all_recommended_costofownership5 = 0;

		$total_opex_focl_all_recommended_costofownership20 = 0;
		$total_opex_mw_all_recommended_costofownership20 = 0;
		$total_opex_sat_all_recommended_costofownership20 = 0;
		$total_opex_cel_all_recommended_costofownership20 = 0;

		$total_costofown5_focl_all_recommended_costofownership5 = 0;
		$total_costofown5_mw_all_recommended_costofownership5 = 0;
		$total_costofown5_sat_all_recommended_costofownership5 = 0;
		$total_costofown5_cel_all_recommended_costofownership5 = 0;

		$total_costofown20_focl_all_recommended_costofownership20 = 0;
		$total_costofown20_mw_all_recommended_costofownership20 = 0;
		$total_costofown20_sat_all_recommended_costofownership20 = 0;
		$total_costofown20_cel_all_recommended_costofownership20 = 0;

		$total_income_all_recommended_costofownership5 = 0;
		$total_income_focl_all_recommended_costofownership5 = 0;
		$total_income_mw_all_recommended_costofownership5 = 0;

		$total_income_all_recommended_costofownership20 = 0;
		$total_income_focl_all_recommended_costofownership20 = 0;
		$total_income_mw_all_recommended_costofownership20 = 0;


//        Total CAPEX of installation and commissioning of solar panels and school power station (for all calculated schools where is no electricity)
//Total OPEX for solar panels and school power station maintenance (for all calculated schools where is no electricity)
//Total 5-years cost of ownership for installation, commissioning and maintenance of solar panels and school power station (for all calculated schools where is no electricity)
//        Free resources? General or per technology recommended? All possible criterias?
//        Income? General or per technology recommended? All possible criterias?


		//NEW DIAGRAMS
//CAPEX of school connection by technology (criteria - cost of ownership, 5-years)
//OPEX of school connection by technology (criteria - cost of ownership, 5-years)
//Cost of ownership of school connection by technology (5-years)
//Income?


		$count_costofownership5_focl = 0;
		$count_costofownership5_mw = 0;
		$count_costofownership5_sat = 0;
		$count_costofownership5_cel = 0;

		$count_costofownership10_focl = 0;
		$count_costofownership10_mw = 0;
		$count_costofownership10_sat = 0;
		$count_costofownership10_cel = 0;

		$count_costofownership15_focl = 0;
		$count_costofownership15_mw = 0;
		$count_costofownership15_sat = 0;
		$count_costofownership15_cel = 0;

		$count_costofownership20_focl = 0;
		$count_costofownership20_mw = 0;
		$count_costofownership20_sat = 0;
		$count_costofownership20_cel = 0;


		$count_npv5_focl = 0;
		$count_npv5_mw = 0;
		$count_npv5_sat = 0;
		$count_npv5_cel = 0;

		$count_npv10_focl = 0;
		$count_npv10_mw = 0;
		$count_npv10_sat = 0;
		$count_npv10_cel = 0;

		$count_npv15_focl = 0;
		$count_npv15_mw = 0;
		$count_npv15_sat = 0;
		$count_npv15_cel = 0;

		$count_npv20_focl = 0;
		$count_npv20_mw = 0;
		$count_npv20_sat = 0;
		$count_npv20_cel = 0;

		$i = 0;
		foreach ($objects as $result_object) {
			$result_object = json_decode($result_object['data']);
			$result = isset($result_object->main) ? $result_object->main : null;
			$required_bandwidth = isset($result_object->required_bandwidth) ? $result_object->required_bandwidth : null;
			$costs = isset($result_object->costs) ? $result_object->costs : null;
			$input = $input_objects[$i];
			$rb = 0;

			if ($required_bandwidth !== null) {
				$rb = isset($required_bandwidth[0]) ? (float)$required_bandwidth[0] : (float)$required_bandwidth;
			}

			$focl_capex = 0;
			$focl_opex = 0;
			$focl_income = 0;
			$mw_capex = 0;
			$mw_opex = 0;
			$mw_income = 0;
			$sat_capex = 0;
			$sat_opex = 0;
			$cel_capex = 0;
			$cel_opex = 0;
			$fb_npv_5 = 0;
			$fb_npv_10 = 0;
			$fb_npv_15 = 0;
			$fb_npv_20 = 0;
			$mw_npv_5 = 0;
			$mw_npv_10 = 0;
			$mw_npv_15 = 0;
			$mw_npv_20 = 0;
			$sat_npv_5 = 0;
			$sat_npv_10 = 0;
			$sat_npv_15 = 0;
			$sat_npv_20 = 0;
			$cel_npv_5 = 0;
			$cel_npv_10 = 0;
			$cel_npv_15 = 0;
			$cel_npv_20 = 0;

			if ($result !== null) {
				$focl_capex = (float)$result[0];
				$focl_opex = (float)$result[1];
				$focl_income = (float)$result[24];
				$mw_capex = (float)$result[2];
				$mw_opex = (float)$result[3];
				$mw_income = (float)$result[25];
				$sat_capex = (float)$result[4];
				$sat_opex = (float)$result[5];
				$cel_capex = (float)$result[6];
				$cel_opex = (float)$result[7];
				$fb_npv_5 = (float)$result[8];
				$fb_npv_10 = (float)$result[12];
				$fb_npv_15 = (float)$result[13];
				$fb_npv_20 = (float)$result[14];
				$mw_npv_5 = (float)$result[9];
				$mw_npv_10 = (float)$result[15];
				$mw_npv_15 = (float)$result[16];
				$mw_npv_20 = (float)$result[17];
				$sat_npv_5 = (float)$result[10];
				$sat_npv_10 = (float)$result[18];
				$sat_npv_15 = (float)$result[19];
				$sat_npv_20 = (float)$result[20];
				$cel_npv_5 = (float)$result[11];
				$cel_npv_10 = (float)$result[21];
				$cel_npv_15 = (float)$result[22];
				$cel_npv_20 = (float)$result[23];
			}

			$lan_capex = 0;
			$lan_opex = 0;

			$tech_comparasion = true;
			$is_celullar_coverage = false;

			switch ($validation->template_class) {
				case 'BBRBOnly':
					if (($input->{7} === "3G") || ($input->{7} === "4G"))
						$is_celullar_coverage = true;
					break;
				case 'ExtendedBBCalcLAN':
					if (($input->{7} === "3G") || ($input->{7} === "4G"))
						$is_celullar_coverage = true;
					break;
				case 'ExtendedBBCalcOnly':
					if (($input->{7} === "3G") || ($input->{7} === "4G"))
						$is_celullar_coverage = true;
					break;
				case 'ExtendedBBRBLAN':
					if (($input->{7} === "3G") || ($input->{7} === "4G"))
						$is_celullar_coverage = true;
					break;
				case 'ExtendedLANOnly':
					$tech_comparasion = false;
					break;
				case 'SimpleBBCalcLAN':
				case 'ExtendedBBCalcLAN':
					if (($input->{7} === "3G") || ($input->{7} === "4G"))
						$is_celullar_coverage = true;
					break;
				case 'SimpleBBCalcOnly':
					if (($input->{7} === "3G") || ($input->{7} === "4G"))
						$is_celullar_coverage = true;
					break;
				case 'SimpleBBRBLAN':
					if (($input->{7} === "3G") || ($input->{7} === "4G"))
						$is_celullar_coverage = true;
					break;
				case 'SimpleLANOnly':
					$tech_comparasion = false;
					break;
				default:

					break;
			}
			$total_required_bandwidth += $rb;

			if ($tech_comparasion) {
				$bFoclParticipated = (bool)(($focl_capex > 0) && ($focl_opex > 0));
				$bMWParticipated = (bool)(($mw_capex > 0) && ($mw_opex > 0));
				$bSatParticipated = (bool)(($sat_capex > 0) && ($sat_opex > 0));
				$bCelParticipated = (bool)((($cel_capex > 0) && ($cel_opex > 0)) && $is_celullar_coverage);

				$case = 0;

				//Case 1 - FOCL - false; MW - false; Sat - false; Cell - false - impossible - die?
				//Case 2 - FOCL - false; MW - false; Sat - false; Cell - true
				if (!$bFoclParticipated && !$bMWParticipated && !$bSatParticipated && $bCelParticipated) {
					$case = 1;
				}

				//Case 3 - FOCL - false; MW - false; Sat - true; Cell - false
				if (!$bFoclParticipated && !$bMWParticipated && $bSatParticipated && !$bCelParticipated) {
					$case = 2;
				}

				//Case 4 - FOCL - false; MW - false; Sat - true; Cell - true
				if (!$bFoclParticipated && !$bMWParticipated && $bSatParticipated && $bCelParticipated) {
					$case = 3;
				}

				//Case 5 - FOCL - false; MW - true; Sat - false; Cell - false
				if (!$bFoclParticipated && $bMWParticipated && !$bSatParticipated && !$bCelParticipated) {
					$case = 4;
				}

				//Case 6 - FOCL - false; MW - true; Sat - false; Cell - true
				if (!$bFoclParticipated && $bMWParticipated && !$bSatParticipated && $bCelParticipated) {
					$case = 5;
				}

				//Case 7 - FOCL - false; MW - true; Sat - true; Cell - false
				if (!$bFoclParticipated && $bMWParticipated && $bSatParticipated && !$bCelParticipated) {
					$case = 6;
				}

				//Case 8 - FOCL - false; MW - true; Sat - true; Cell - true
				if (!$bFoclParticipated && $bMWParticipated && $bSatParticipated && $bCelParticipated) {
					$case = 7;
				}

				//Case 9 - FOCL - true; MW - false; Sat - false; Cell - false
				if ($bFoclParticipated && !$bMWParticipated && !$bSatParticipated && !$bCelParticipated) {
					$case = 8;
				}

				//Case 10 - FOCL - true; MW - false; Sat - false; Cell - true
				if ($bFoclParticipated && !$bMWParticipated && !$bSatParticipated && $bCelParticipated) {
					$case = 9;
				}

				//Case 11 - FOCL - true; MW - false; Sat - true; Cell - false
				if ($bFoclParticipated && !$bMWParticipated && $bSatParticipated && !$bCelParticipated) {
					$case = 10;
				}

				//Case 12 - FOCL - true; MW - false; Sat - true; Cell - true
				if ($bFoclParticipated && !$bMWParticipated && $bSatParticipated && $bCelParticipated) {
					$case = 11;
				}

				//Case 13 - FOCL - true; MW - true; Sat - false; Cell - false
				if ($bFoclParticipated && $bMWParticipated && !$bSatParticipated && !$bCelParticipated) {
					$case = 12;
				}

				//Case 14 - FOCL - true; MW - true; Sat - false; Cell - true
				if ($bFoclParticipated && $bMWParticipated && !$bSatParticipated && $bCelParticipated) {
					$case = 13;
				}

				//Case 15 - FOCL - true; MW - true; Sat - true; Cell - false
				if ($bFoclParticipated && $bMWParticipated && $bSatParticipated && !$bCelParticipated) {
					$case = 14;
				}

				//Case 16 - FOCL - true; MW - true; Sat - true; Cell - true
				if ($bFoclParticipated && $bMWParticipated && $bSatParticipated && $bCelParticipated) {
					$case = 15;
				}

				if ($case > 0) {
					//Winner by Cost of ownership 5 years
					$focl_costown5 = $focl_capex + 5 * $focl_opex;
					$mw_costown5 = $mw_capex + 5 * $mw_opex;
					$sat_costown5 = $sat_capex + 5 * $sat_opex;
					$cel_costown5 = $cel_capex + 5 * $cel_opex;

					$winner5 = CalculationsModel::who_is_the_winner_by_costofownership($case, $focl_costown5, $mw_costown5, $sat_costown5, $cel_costown5);

					switch ($winner5) {
						case 'Fiber':
							$total_capex_for_all_recommended += $focl_capex;
							$total_capex_focl_all_recommended_costofownership5 += $focl_capex;

							$total_opex_for_all_recommended += $focl_opex;
							$total_opex_focl_all_recommended_costofownership5 += $focl_opex;

							$total_costofownership5_for_all_recommended += $focl_costown5;
							$total_costofown5_focl_all_recommended_costofownership5 += $focl_costown5;

							$total_income_all_recommended_costofownership5 += $focl_income;
							$total_income_focl_all_recommended_costofownership5 += $focl_income;

							$count_costofownership5_focl++;

							break;
						case 'Microwave':
							$total_capex_for_all_recommended += $mw_capex;
							$total_capex_mw_all_recommended_costofownership5 += $mw_capex;

							$total_opex_for_all_recommended += $mw_opex;
							$total_opex_mw_all_recommended_costofownership5 += $mw_opex;

							$total_costofownership5_for_all_recommended += $mw_costown5;
							$total_costofown5_mw_all_recommended_costofownership5 += $mw_costown5;

							$total_income_all_recommended_costofownership5 += $mw_income;
							$total_income_mw_all_recommended_costofownership5 += $mw_income;


							$count_costofownership5_mw++;
							break;
						case 'Satellite':
							$total_capex_for_all_recommended += $sat_capex;
							$total_capex_sat_all_recommended_costofownership5 += $sat_capex;

							$total_opex_for_all_recommended += $sat_opex;
							$total_opex_sat_all_recommended_costofownership5 += $sat_opex;

							$total_costofownership5_for_all_recommended += $sat_costown5;
							$total_costofown5_sat_all_recommended_costofownership5 += $sat_costown5;

							$count_costofownership5_sat++;
							break;
						case 'Cellular':
							$total_capex_for_all_recommended += $cel_capex;
							$total_capex_cel_all_recommended_costofownership5 += $cel_capex;

							$total_opex_for_all_recommended += $cel_opex;
							$total_opex_cel_all_recommended_costofownership5 += $cel_opex;

							$total_costofownership5_for_all_recommended += $cel_costown5;
							$total_costofown5_cel_all_recommended_costofownership5 += $cel_costown5;

							$count_costofownership5_cel++;
							break;
						default:
							break;
					}

					//Winner by Cost of ownership 10 years
					$focl_costown10 = $focl_capex + 10 * $focl_opex;
					$mw_costown10 = $mw_capex + 10 * $mw_opex;
					$sat_costown10 = $sat_capex + 10 * $sat_opex;
					$cel_costown10 = $cel_capex + 10 * $cel_opex;

					$winner10 = CalculationsModel::who_is_the_winner_by_costofownership($case, $focl_costown10, $mw_costown10, $sat_costown10, $cel_costown10);

					switch ($winner10) {
						case 'Fiber':
							$total_costofownership10_for_all_recommended += $focl_costown10;
							$count_costofownership10_focl++;
							break;
						case 'Microwave':
							$total_costofownership10_for_all_recommended += $mw_costown10;
							$count_costofownership10_mw++;
							break;
						case 'Satellite':
							$total_costofownership10_for_all_recommended += $sat_costown10;
							$count_costofownership10_sat++;
							break;
						case 'Cellular':
							$total_costofownership10_for_all_recommended += $cel_costown10;
							$count_costofownership10_cel++;
							break;
						default:
							break;
					}


					//Winner by Cost of ownership 15 years
					$focl_costown15 = $focl_capex + 15 * $focl_opex;
					$mw_costown15 = $mw_capex + 15 * $mw_opex;
					$sat_costown15 = $sat_capex + 15 * $sat_opex;
					$cel_costown15 = $cel_capex + 15 * $cel_opex;

					$winner15 = CalculationsModel::who_is_the_winner_by_costofownership($case, $focl_costown15, $mw_costown15, $sat_costown15, $cel_costown15);

					switch ($winner15) {
						case 'Fiber':
							$total_costofownership15_for_all_recommended += $focl_costown15;
							$count_costofownership15_focl++;
							break;
						case 'Microwave':
							$total_costofownership15_for_all_recommended += $mw_costown15;
							$count_costofownership15_mw++;
							break;
						case 'Satellite':
							$total_costofownership15_for_all_recommended += $sat_costown15;
							$count_costofownership15_sat++;
							break;
						case 'Cellular':
							$total_costofownership15_for_all_recommended += $cel_costown15;
							$count_costofownership15_cel++;
							break;
						default:
							break;
					}


					//Winner by Cost of ownership 20 years
					$focl_costown20 = $focl_capex + 20 * $focl_opex;
					$mw_costown20 = $mw_capex + 20 * $mw_opex;
					$sat_costown20 = $sat_capex + 20 * $sat_opex;
					$cel_costown20 = $cel_capex + 20 * $cel_opex;

					$winner20 = CalculationsModel::who_is_the_winner_by_costofownership($case, $focl_costown20, $mw_costown20, $sat_costown20, $cel_costown20);

					switch ($winner20) {
						case 'Fiber':
							$total_costofownership20_for_all_recommended += $focl_costown20;
							$total_costofown20_focl_all_recommended_costofownership20 += $focl_costown20;

							$total_capex_focl_all_recommended_costofownership20 += $focl_capex;
							$total_opex_focl_all_recommended_costofownership20 += $focl_opex;

							$total_income_all_recommended_costofownership20 += $focl_income;
							$total_income_focl_all_recommended_costofownership20 += $focl_income;


							$count_costofownership20_focl++;
							break;
						case 'Microwave':
							$total_costofownership20_for_all_recommended += $mw_costown20;
							$total_costofown20_mw_all_recommended_costofownership20 += $mw_costown20;

							$total_capex_mw_all_recommended_costofownership20 += $mw_capex;
							$total_opex_mw_all_recommended_costofownership20 += $mw_opex;

							$total_income_all_recommended_costofownership20 += $mw_income;
							$total_income_mw_all_recommended_costofownership20 += $mw_income;

							$count_costofownership20_mw++;
							break;
						case 'Satellite':
							$total_costofownership20_for_all_recommended += $sat_costown20;
							$total_costofown20_sat_all_recommended_costofownership20 += $sat_costown20;

							$total_capex_sat_all_recommended_costofownership20 += $sat_capex;
							$total_opex_sat_all_recommended_costofownership20 += $sat_opex;

							$count_costofownership20_sat++;
							break;
						case 'Cellular':
							$total_costofownership20_for_all_recommended += $cel_costown20;
							$total_costofown20_cel_all_recommended_costofownership20 += $cel_costown20;

							$total_capex_cel_all_recommended_costofownership20 += $cel_capex;
							$total_opex_cel_all_recommended_costofownership20 += $cel_opex;

							$count_costofownership20_cel++;
							break;
						default:
							break;
					}


					//Winner by NPV 5 years
					$npv_winner5 = CalculationsModel::who_is_the_winner_by_npv($case, $fb_npv_5, $mw_npv_5, $sat_npv_5, $cel_npv_5);

					switch ($npv_winner5) {
						case 'Fiber':
							$total_npv5_for_all_recommended += $fb_npv_5;
							$count_npv5_focl++;
							break;
						case 'Microwave':
							$total_npv5_for_all_recommended += $mw_npv_5;
							$count_npv5_mw++;
							break;
						case 'Satellite':
							$total_npv5_for_all_recommended += $sat_npv_5;
							$count_npv5_sat++;
							break;
						case 'Cellular':
							$total_npv5_for_all_recommended += $cel_npv_5;
							$count_npv5_cel++;
							break;
						default:
							break;
					}


					//Winner by NPV 10 years
					$npv_winner10 = CalculationsModel::who_is_the_winner_by_npv($case, $fb_npv_10, $mw_npv_10, $sat_npv_10, $cel_npv_10);

					switch ($npv_winner10) {
						case 'Fiber':
							$total_npv10_for_all_recommended += $fb_npv_10;
							$count_npv10_focl++;
							break;
						case 'Microwave':
							$total_npv10_for_all_recommended += $mw_npv_10;
							$count_npv10_mw++;
							break;
						case 'Satellite':
							$total_npv10_for_all_recommended += $sat_npv_10;
							$count_npv10_sat++;
							break;
						case 'Cellular':
							$total_npv10_for_all_recommended += $cel_npv_10;
							$count_npv10_cel++;
							break;
						default:
							break;
					}


					//Winner by NPV 15 years
					$npv_winner15 = CalculationsModel::who_is_the_winner_by_npv($case, $fb_npv_15, $mw_npv_15, $sat_npv_15, $cel_npv_15);

					switch ($npv_winner15) {
						case 'Fiber':
							$total_npv15_for_all_recommended += $fb_npv_15;
							$count_npv15_focl++;
							break;
						case 'Microwave':
							$total_npv15_for_all_recommended += $mw_npv_15;
							$count_npv15_mw++;
							break;
						case 'Satellite':
							$total_npv15_for_all_recommended += $sat_npv_15;
							$count_npv15_sat++;
							break;
						case 'Cellular':
							$total_npv15_for_all_recommended += $cel_npv_15;
							$count_npv15_cel++;
							break;
						default:
							break;
					}

					//Winner by NPV 20 years
					$npv_winner20 = CalculationsModel::who_is_the_winner_by_npv($case, $fb_npv_20, $mw_npv_20, $sat_npv_20, $cel_npv_20);

					switch ($npv_winner20) {
						case 'Fiber':
							$total_npv20_for_all_recommended += $fb_npv_20;
							$count_npv20_focl++;
							break;
						case 'Microwave':
							$total_npv20_for_all_recommended += $mw_npv_20;
							$count_npv20_mw++;
							break;
						case 'Satellite':
							$total_npv20_for_all_recommended += $sat_npv_20;
							$count_npv20_sat++;
							break;
						case 'Cellular':
							$total_npv20_for_all_recommended += $cel_npv_20;
							$count_npv20_cel++;
							break;
						default:
							break;
					}

				}

			}

			$i++;
		}

		if (($count_costofownership5_focl > 0) || ($count_costofownership5_mw > 0) || ($count_costofownership5_sat > 0) || ($count_costofownership5_cel > 0)) {
			$diagrams['count_costofownership5'] = [];
			if (!empty($count_costofownership5_focl)) $diagrams['count_costofownership5'][] = ["text" => "dia_focl", "values" => [round($count_costofownership5_focl,2)]];
			if (!empty($count_costofownership5_mw)) $diagrams['count_costofownership5'][] = ["text" => "dia_mw", "values" => [round($count_costofownership5_mw,2)]];
			if (!empty($count_costofownership5_sat)) $diagrams['count_costofownership5'][] = ["text" => "dia_sat", "values" => [round($count_costofownership5_sat,2)]];
			if (!empty($count_costofownership5_cel)) $diagrams['count_costofownership5'][] = ["text" => "dia_cel", "values" => [round($count_costofownership5_cel,2)]];
		}

		if (($count_costofownership10_focl > 0) || ($count_costofownership10_mw > 0) ||
			($count_costofownership10_sat > 0) || ($count_costofownership10_cel > 0)) {
			$diagrams['count_costofownership10'] = [];
			if (!empty($count_costofownership10_focl)) $diagrams['count_costofownership10'][] = ["text" => "dia_focl", "values" => [round($count_costofownership10_focl,2)]];
			if (!empty($count_costofownership10_mw)) $diagrams['count_costofownership10'][] = ["text" => "dia_mw", "values" => [round($count_costofownership10_mw,2)]];
			if (!empty($count_costofownership10_sat)) $diagrams['count_costofownership10'][] = ["text" => "dia_sat", "values" => [round($count_costofownership10_sat,2)]];
			if (!empty($count_costofownership10_cel)) $diagrams['count_costofownership10'][] = ["text" => "dia_cel", "values" => [round($count_costofownership10_cel,2)]];
		}

		if (($count_costofownership15_focl > 0) || ($count_costofownership15_mw > 0) ||
			($count_costofownership15_sat > 0) || ($count_costofownership15_cel > 0)) {
			$diagrams['count_costofownership15'] = [];
			if (!empty($count_costofownership15_focl)) $diagrams['count_costofownership15'][] = ["text" => "dia_focl", "values" => [round($count_costofownership15_focl,2)]];
			if (!empty($count_costofownership15_mw)) $diagrams['count_costofownership15'][] = ["text" => "dia_mw", "values" => [round($count_costofownership15_mw,2)]];
			if (!empty($count_costofownership15_sat)) $diagrams['count_costofownership15'][] = ["text" => "dia_sat", "values" => [round($count_costofownership15_sat,2)]];
			if (!empty($count_costofownership15_cel)) $diagrams['count_costofownership15'][] = ["text" => "dia_cel", "values" => [round($count_costofownership15_cel,2)]];
		}

		if (($count_costofownership20_focl > 0) || ($count_costofownership20_mw > 0) ||
			($count_costofownership20_sat > 0) || ($count_costofownership20_cel > 0)) {
			$diagram['count_costofownership20'] = [];
			if (!empty($count_costofownership20_focl)) $diagrams['count_costofownership20'][] = ["text" => "dia_focl", "values" => [round($count_costofownership20_focl,2)]];
			if (!empty($count_costofownership20_mw)) $diagrams['count_costofownership20'][] = ["text" => "dia_mw", "values" => [round($count_costofownership20_mw,2)]];
			if (!empty($count_costofownership20_sat)) $diagrams['count_costofownership20'][] = ["text" => "dia_sat", "values" => [round($count_costofownership20_sat,2)]];
			if (!empty($count_costofownership20_cel)) $diagrams['count_costofownership20'][] = ["text" => "dia_cel", "values" => [round($count_costofownership20_cel,2)]];
		}

		if (($count_npv5_focl > 0) || ($count_npv5_mw > 0) ||
			($count_npv5_sat > 0) || ($count_npv5_cel > 0)) {
			$diagrams['count_npv5'] = [];
			if (!empty($count_npv5_focl)) $diagrams['count_npv5'][] = ["text" => "dia_focl", "values" => [round($count_npv5_focl,2)]];
			if (!empty($count_npv5_mw)) $diagrams['count_npv5'][] = ["text" => "dia_mw", "values" => [round($count_npv5_mw,2)]];
			if (!empty($count_npv5_sat)) $diagrams['count_npv5'][] = ["text" => "dia_sat", "values" => [round($count_npv5_sat,2)]];
			if (!empty($count_npv5_cel)) $diagrams['count_npv5'][] = ["text" => "dia_cel", "values" => [round($count_npv5_cel,2)]];
		}


		if (($count_npv10_focl > 0) || ($count_npv10_mw > 0) ||
			($count_npv10_sat > 0) || ($count_npv10_cel > 0)) {
			$diagrams['count_npv10'] = [];
			if (!empty($count_npv10_focl)) $diagrams['count_npv10'][] = ["text" => "dia_focl", "values" => [round($count_npv10_focl,2)]];
			if (!empty($count_npv10_mw)) $diagrams['count_npv10'][] = ["text" => "dia_mw", "values" => [round($count_npv10_mw,2)]];
			if (!empty($count_npv10_sat)) $diagrams['count_npv10'][] = ["text" => "dia_sat", "values" => [round($count_npv10_sat,2)]];
			if (!empty($count_npv10_cel)) $diagrams['count_npv10'][] = ["text" => "dia_cel", "values" => [round($count_npv10_cel,2)]];
		}

		if (($count_npv15_focl > 0) || ($count_npv15_mw > 0) ||
			($count_npv15_sat > 0) || ($count_npv15_cel > 0)) {
			$diagrams['count_npv15'] = [];
			if (!empty($count_npv15_focl)) $diagrams['count_npv15'][] = ["text" => "dia_focl", "values" => [round($count_npv15_focl,2)]];
			if (!empty($count_npv15_mw)) $diagrams['count_npv15'][] = ["text" => "dia_mw", "values" => [round($count_npv15_mw,2)]];
			if (!empty($count_npv15_sat)) $diagrams['count_npv15'][] = ["text" => "dia_sat", "values" => [round($count_npv15_sat,2)]];
			if (!empty($count_npv15_cel)) $diagrams['count_npv15'][] = ["text" => "dia_cel", "values" => [round($count_npv15_cel,2)]];
		}

		if (($count_npv20_focl > 0) || ($count_npv20_mw > 0) ||
			($count_npv20_sat > 0) || ($count_npv20_cel > 0)) {
			$diagrams['count_npv20'] = [];
			if (!empty($count_npv20_focl)) $diagrams['count_npv20'][] = ["text" => "dia_focl", "values" => [round($count_npv20_focl,2)]];
			if (!empty($count_npv20_mw)) $diagrams['count_npv20'][] = ["text" => "dia_mw", "values" => [round($count_npv20_mw,2)]];
			if (!empty($count_npv20_sat)) $diagrams['count_npv20'][] = ["text" => "dia_sat", "values" => [round($count_npv20_sat,2)]];
			if (!empty($count_npv20_cel)) $diagrams['count_npv20'][] = ["text" => "dia_cel", "values" => [round($count_npv20_cel,2)]];
		}

		if (($total_capex_focl_all_recommended_costofownership5 > 0) ||
			($total_capex_mw_all_recommended_costofownership5 > 0) ||
			($total_capex_sat_all_recommended_costofownership5 > 0) ||
			($total_capex_cel_all_recommended_costofownership5 > 0)) {
			if (!empty($total_capex_focl_all_recommended_costofownership5)) $diagrams['money_capex5'][] = ["text" => "dia_focl", "values" => [round($total_capex_focl_all_recommended_costofownership5,2)]];

			if (!empty($total_capex_mw_all_recommended_costofownership5)) $diagrams['money_capex5'][] = ["text" => "dia_mw", "values" => [round($total_capex_mw_all_recommended_costofownership5,2)]];

			if (!empty($total_capex_sat_all_recommended_costofownership5)) $diagrams['money_capex5'][] = ["text" => "dia_sat", "values" => [round($total_capex_sat_all_recommended_costofownership5,2)]];

			if (!empty($total_capex_cel_all_recommended_costofownership5)) $diagrams['money_capex5'][] = ["text" => "dia_cel", "values" => [round($total_capex_cel_all_recommended_costofownership5,2)]];
		}

		if (($total_capex_focl_all_recommended_costofownership20 > 0) ||
			($total_capex_mw_all_recommended_costofownership20 > 0) ||
			($total_capex_sat_all_recommended_costofownership20 > 0) ||
			($total_capex_cel_all_recommended_costofownership20 > 0)) {
			if (!empty($total_capex_focl_all_recommended_costofownership20)) $diagrams['money_capex20'][] = ["text" => "dia_focl", "values" => [round($total_capex_focl_all_recommended_costofownership20,2)]];

			if (!empty($total_capex_mw_all_recommended_costofownership20)) $diagrams['money_capex20'][] = ["text" => "dia_mw", "values" => [round($total_capex_mw_all_recommended_costofownership20,2)]];

			if (!empty($total_capex_sat_all_recommended_costofownership20)) $diagrams['money_capex20'][] = ["text" => "dia_sat", "values" => [round($total_capex_sat_all_recommended_costofownership20,2)]];

			if (!empty($total_capex_cel_all_recommended_costofownership20)) $diagrams['money_capex20'][] = ["text" => "dia_cel", "values" => [round($total_capex_cel_all_recommended_costofownership20,2)]];
		}


		if (($total_opex_focl_all_recommended_costofownership5 > 0) ||
			($total_opex_mw_all_recommended_costofownership5 > 0) ||
			($total_opex_sat_all_recommended_costofownership5 > 0) ||
			($total_opex_cel_all_recommended_costofownership5 > 0)) {
			if (!empty($total_opex_focl_all_recommended_costofownership5)) $diagrams['money_opex5'][] = ["text" => "dia_focl", "values" => [round($total_opex_focl_all_recommended_costofownership5,2)]];

			if (!empty($total_opex_mw_all_recommended_costofownership5)) $diagrams['money_opex5'][] = ["text" => "dia_mw", "values" => [round($total_opex_mw_all_recommended_costofownership5,2)]];

			if (!empty($total_opex_sat_all_recommended_costofownership5)) $diagrams['money_opex5'][] = ["text" => "dia_sat", "values" => [round($total_opex_sat_all_recommended_costofownership5,2)]];

			if (!empty($total_opex_cel_all_recommended_costofownership5)) $diagrams['money_opex5'][] = ["text" => "dia_cel", "values" => [round($total_opex_cel_all_recommended_costofownership5,2)]];
		}

		if (($total_opex_focl_all_recommended_costofownership20 > 0) ||
			($total_opex_mw_all_recommended_costofownership20 > 0) ||
			($total_opex_sat_all_recommended_costofownership20 > 0) ||
			($total_opex_cel_all_recommended_costofownership20 > 0)) {
			if (!empty($total_opex_focl_all_recommended_costofownership20)) $diagrams['money_opex20'][] = ["text" => "dia_focl", "values" => [round($total_opex_focl_all_recommended_costofownership20,2)]];

			if (!empty($total_opex_mw_all_recommended_costofownership20)) $diagrams['money_opex20'][] = ["text" => "dia_mw", "values" => [round($total_opex_mw_all_recommended_costofownership20,2)]];

			if (!empty($total_opex_sat_all_recommended_costofownership20)) $diagrams['money_opex20'][] = ["text" => "dia_sat", "values" => [round($total_opex_sat_all_recommended_costofownership20,2)]];

			if (!empty($total_opex_cel_all_recommended_costofownership20)) $diagrams['money_opex20'][] = ["text" => "dia_cel", "values" => [round($total_opex_cel_all_recommended_costofownership20,2)]];
		}

		if (($total_costofown5_focl_all_recommended_costofownership5 > 0) ||
			($total_costofown5_mw_all_recommended_costofownership5 > 0) ||
			($total_costofown5_sat_all_recommended_costofownership5 > 0) ||
			($total_costofown5_cel_all_recommended_costofownership5 > 0)) {
			if (!empty($total_costofown5_focl_all_recommended_costofownership5)) $diagrams['money_costofown5'][] = ["text" => "dia_focl", "values" => [round($total_costofown5_focl_all_recommended_costofownership5,2)]];

			if (!empty($total_costofown5_mw_all_recommended_costofownership5)) $diagrams['money_costofown5'][] = ["text" => "dia_mw", "values" => [round($total_costofown5_mw_all_recommended_costofownership5,2)]];

			if (!empty($total_costofown5_sat_all_recommended_costofownership5)) $diagrams['money_costofown5'][] = ["text" => "dia_sat", "values" => [round($total_costofown5_sat_all_recommended_costofownership5,2)]];

			if (!empty($total_costofown5_cel_all_recommended_costofownership5)) $diagrams['money_costofown5'][] = ["text" => "dia_cel", "values" => [round($total_costofown5_cel_all_recommended_costofownership5,2)]];
		}


		if (($total_costofown20_focl_all_recommended_costofownership20 > 0) ||
			($total_costofown20_mw_all_recommended_costofownership20 > 0) ||
			($total_costofown20_sat_all_recommended_costofownership20 > 0) ||
			($total_costofown20_cel_all_recommended_costofownership20 > 0)) {
			if (!empty($total_costofown20_focl_all_recommended_costofownership20)) $diagrams['money_costofown20'][] = ["text" => "dia_focl", "values" => [round($total_costofown20_focl_all_recommended_costofownership20,2)]];

			if (!empty($total_costofown20_mw_all_recommended_costofownership20)) $diagrams['money_costofown20'][] = ["text" => "dia_mw", "values" => [round($total_costofown20_mw_all_recommended_costofownership20,2)]];

			if (!empty($total_costofown20_sat_all_recommended_costofownership20)) $diagrams['money_costofown20'][] = ["text" => "dia_sat", "values" => [round($total_costofown20_sat_all_recommended_costofownership20,2)]];

			if (!empty($total_costofown20_cel_all_recommended_costofownership20)) $diagrams['money_costofown20'][] = ["text" => "dia_cel", "values" => [round($total_costofown20_cel_all_recommended_costofownership20,2)]];
		}

		if ($total_income_focl_all_recommended_costofownership5 > 0 || $total_income_mw_all_recommended_costofownership5 > 0) {
			if (!empty($total_income_focl_all_recommended_costofownership5)) $diagrams['income_costofown5'][] = ["text" => "dia_focl", "values" => [round($total_income_focl_all_recommended_costofownership5,2)]];
			if (!empty($total_income_mw_all_recommended_costofownership5)) $diagrams['income_costofown5'][] = ["text" => "dia_mw", "values" => [round($total_income_mw_all_recommended_costofownership5,2)]];
		}

		if ($total_income_focl_all_recommended_costofownership20 > 0 || $total_income_mw_all_recommended_costofownership20 > 0) {
			if (!empty($total_income_focl_all_recommended_costofownership20)) $diagrams['income_costofown20'][] = ["text" => "dia_focl", "values" => [round($total_income_focl_all_recommended_costofownership20,2)]];
			if (!empty($total_income_mw_all_recommended_costofownership20)) $diagrams['income_costofown20'][] = ["text" => "dia_mw", "values" => [round($total_income_mw_all_recommended_costofownership20,2)]];
		}

		if ($total_required_bandwidth > 0) {
			$summary[] = ['name' => 'total_required_bandwidth', 'value' => round($total_required_bandwidth, 2), 'units' => 'value_mbps'];
			$mean_required_bandwidth = $total_required_bandwidth / $validation->valid_objects;
			$summary[] = ['name' => 'mean_required_bandwidth', 'value' => round($mean_required_bandwidth, 2), 'units' => 'value_mbps'];
		}

		if ($total_capex_for_all_recommended > 0) {
			$summary[] = ['name' => 'total_capex_for_all_recommended', 'value' => round($total_capex_for_all_recommended, 2), 'units' => 'value_usd'];
			$mean_capex_for_all_recommended = $total_capex_for_all_recommended / $validation->valid_objects;
			$summary[] = ['name' => 'mean_capex_for_all_recommended', 'value' => round($mean_capex_for_all_recommended, 2), 'units' => 'value_usd'];
		}

		if ($total_opex_for_all_recommended > 0) {
			$summary[] = ['name' => 'total_opex_for_all_recommended', 'value' => round($total_opex_for_all_recommended, 2), 'units' => 'value_usdperyear'];
			$mean_opex_for_all_recommended = $total_opex_for_all_recommended / $validation->valid_objects;
			$summary[] = ['name' => 'mean_opex_for_all_recommended', 'value' => round($mean_opex_for_all_recommended, 2), 'units' => 'value_usdperyear'];
		}

		if ($total_costofownership5_for_all_recommended > 0) {
			$summary[] = ['name' => 'total_costofownership5_for_all_recommended', 'value' => round($total_costofownership5_for_all_recommended, 2), 'units' => 'value_usd'];
			$mean_costofownership5_for_all_recommended = $total_costofownership5_for_all_recommended / $validation->valid_objects;
			$summary[] = ['name' => 'mean_costofownership5_for_all_recommended', 'value' => round($mean_costofownership5_for_all_recommended, 2), 'units' => 'value_usd'];
		}

		if ($total_costofownership10_for_all_recommended > 0) {
			$summary[] = ['name' => 'total_costofownership10_for_all_recommended', 'value' => round($total_costofownership10_for_all_recommended, 2), 'units' => 'value_usd'];
			$mean_costofownership10_for_all_recommended = $total_costofownership10_for_all_recommended / $validation->valid_objects;
			$summary[] = ['name' => 'mean_costofownership10_for_all_recommended', 'value' => round($mean_costofownership10_for_all_recommended, 2), 'units' => 'value_usd'];
		}

		if ($total_costofownership15_for_all_recommended > 0) {
			$summary[] = ['name' => 'total_costofownership15_for_all_recommended', 'value' => round($total_costofownership15_for_all_recommended, 2), 'units' => 'value_usd'];
			$mean_costofownership15_for_all_recommended = $total_costofownership15_for_all_recommended / $validation->valid_objects;
			$summary[] = ['name' => 'mean_costofownership15_for_all_recommended', 'value' => round($mean_costofownership15_for_all_recommended, 2), 'units' => 'value_usd'];
		}

		if ($total_costofownership20_for_all_recommended > 0) {
			$summary[] = ['name' => 'total_costofownership20_for_all_recommended', 'value' => round($total_costofownership20_for_all_recommended, 2), 'units' => 'value_usd'];
			$mean_costofownership20_for_all_recommended = $total_costofownership20_for_all_recommended / $validation->valid_objects;
			$summary[] = ['name' => 'mean_costofownership20_for_all_recommended', 'value' => round($mean_costofownership20_for_all_recommended, 2), 'units' => 'value_usd'];
		}

		if ($total_npv5_for_all_recommended != 0) {
			$summary[] = ['name' => 'total_npv5_for_all_recommended', 'value' => round($total_npv5_for_all_recommended, 2), 'units' => 'value_usd'];
			$mean_npv5_for_all_recommended = $total_npv5_for_all_recommended / $validation->valid_objects;
			$summary[] = ['name' => 'mean_npv5_for_all_recommended', 'value' => round($mean_npv5_for_all_recommended, 2), 'units' => 'value_usd'];
		}

		if ($total_npv10_for_all_recommended != 0) {
			$summary[] = ['name' => 'total_npv10_for_all_recommended', 'value' => round($total_npv10_for_all_recommended, 2), 'units' => 'value_usd'];
			$mean_npv10_for_all_recommended = $total_npv10_for_all_recommended / $validation->valid_objects;
			$summary[] = ['name' => 'mean_npv10_for_all_recommended', 'value' => round($mean_npv10_for_all_recommended, 2), 'units' => 'value_usd'];
		}

		if ($total_npv15_for_all_recommended != 0) {
			$summary[] = ['name' => 'total_npv15_for_all_recommended', 'value' => round($total_npv15_for_all_recommended, 2), 'units' => 'value_usd'];
			$mean_npv15_for_all_recommended = $total_npv15_for_all_recommended / $validation->valid_objects;
			$summary[] = ['name' => 'mean_npv15_for_all_recommended', 'value' => round($mean_npv15_for_all_recommended, 2), 'units' => 'value_usd'];
		}

		if ($total_npv20_for_all_recommended != 0) {
			$summary[] = ['name' => 'total_npv20_for_all_recommended', 'value' => round($total_npv20_for_all_recommended, 2), 'units' => 'value_usd'];
			$mean_npv20_for_all_recommended = $total_npv20_for_all_recommended / $validation->valid_objects;
			$summary[] = ['name' => 'mean_npv20_for_all_recommended', 'value' => round($mean_npv20_for_all_recommended, 2), 'units' => 'value_usd'];
		}

		//NEW RESULTS

		if ($total_capex_focl_all_recommended_costofownership5 != 0) {
			$summary[] = ['name' => 'total_capex_focl_all_recommended_costofownership5', 'value' => round($total_capex_focl_all_recommended_costofownership5, 2), 'units' => 'value_usd'];
		}

		if ($total_capex_mw_all_recommended_costofownership5 != 0) {
			$summary[] = ['name' => 'total_capex_mw_all_recommended_costofownership5', 'value' => round($total_capex_mw_all_recommended_costofownership5, 2), 'units' => 'value_usd'];
		}

		if ($total_capex_sat_all_recommended_costofownership5 != 0) {
			$summary[] = ['name' => 'total_capex_sat_all_recommended_costofownership5', 'value' => round($total_capex_sat_all_recommended_costofownership5, 2), 'units' => 'value_usd'];
		}

		if ($total_capex_cel_all_recommended_costofownership5 != 0) {
			$summary[] = ['name' => 'total_capex_cel_all_recommended_costofownership5', 'value' => round($total_capex_cel_all_recommended_costofownership5, 2), 'units' => 'value_usd'];
		}

		if ($total_capex_focl_all_recommended_costofownership20 != 0) {
			$summary[] = ['name' => 'total_capex_focl_all_recommended_costofownership20', 'value' => round($total_capex_focl_all_recommended_costofownership20, 2), 'units' => 'value_usd'];
		}

		if ($total_capex_mw_all_recommended_costofownership20 != 0) {
			$summary[] = ['name' => 'total_capex_mw_all_recommended_costofownership20', 'value' => round($total_capex_mw_all_recommended_costofownership20, 2), 'units' => 'value_usd'];
		}

		if ($total_capex_sat_all_recommended_costofownership20 != 0) {
			$summary[] = ['name' => 'total_capex_sat_all_recommended_costofownership20', 'value' => round($total_capex_sat_all_recommended_costofownership20, 2), 'units' => 'value_usd'];
		}

		if ($total_capex_cel_all_recommended_costofownership20 != 0) {
			$summary[] = ['name' => 'total_capex_cel_all_recommended_costofownership20', 'value' => round($total_capex_cel_all_recommended_costofownership20, 2), 'units' => 'value_usd'];
		}

		if ($total_opex_focl_all_recommended_costofownership5 != 0) {
			$summary[] = ['name' => 'total_opex_focl_all_recommended_costofownership5', 'value' => round($total_opex_focl_all_recommended_costofownership5, 2), 'units' => 'value_usdperyear'];
		}

		if ($total_opex_mw_all_recommended_costofownership5 != 0) {
			$summary[] = ['name' => 'total_opex_mw_all_recommended_costofownership5', 'value' => round($total_opex_mw_all_recommended_costofownership5, 2), 'units' => 'value_usdperyear'];
		}

		if ($total_opex_sat_all_recommended_costofownership5 != 0) {
			$summary[] = ['name' => 'total_opex_sat_all_recommended_costofownership5', 'value' => round($total_opex_sat_all_recommended_costofownership5, 2), 'units' => 'value_usdperyear'];
		}

		if ($total_opex_cel_all_recommended_costofownership5 != 0) {
			$summary[] = ['name' => 'total_opex_cel_all_recommended_costofownership5', 'value' => round($total_opex_cel_all_recommended_costofownership5, 2), 'units' => 'value_usdperyear'];
		}

		if ($total_opex_focl_all_recommended_costofownership20 != 0) {
			$summary[] = ['name' => 'total_opex_focl_all_recommended_costofownership20', 'value' => round($total_opex_focl_all_recommended_costofownership20, 2), 'units' => 'value_usdperyear'];
		}

		if ($total_opex_mw_all_recommended_costofownership20 != 0) {
			$summary[] = ['name' => 'total_opex_mw_all_recommended_costofownership20', 'value' => round($total_opex_mw_all_recommended_costofownership20, 2), 'units' => 'value_usdperyear'];
		}

		if ($total_opex_sat_all_recommended_costofownership20 != 0) {
			$summary[] = ['name' => 'total_opex_sat_all_recommended_costofownership20', 'value' => round($total_opex_sat_all_recommended_costofownership20, 2), 'units' => 'value_usdperyear'];
		}

		if ($total_opex_cel_all_recommended_costofownership20 != 0) {
			$summary[] = ['name' => 'total_opex_cel_all_recommended_costofownership20', 'value' => round($total_opex_cel_all_recommended_costofownership20, 2), 'units' => 'value_usdperyear'];
		}

		if ($total_costofown5_focl_all_recommended_costofownership5 != 0) {
			$summary[] = ['name' => 'total_costofown5_focl_all_recommended_costofownership5', 'value' => round($total_costofown5_focl_all_recommended_costofownership5, 2), 'units' => 'value_usd'];
		}

		if ($total_costofown5_mw_all_recommended_costofownership5 != 0) {
			$summary[] = ['name' => 'total_costofown5_mw_all_recommended_costofownership5', 'value' => round($total_costofown5_mw_all_recommended_costofownership5, 2), 'units' => 'value_usd'];
		}

		if ($total_costofown5_sat_all_recommended_costofownership5 != 0) {
			$summary[] = ['name' => 'total_costofown5_sat_all_recommended_costofownership5', 'value' => round($total_costofown5_sat_all_recommended_costofownership5, 2), 'units' => 'value_usd'];
		}

		if ($total_costofown5_cel_all_recommended_costofownership5 != 0) {
			$summary[] = ['name' => 'total_costofown5_cel_all_recommended_costofownership5', 'value' => round($total_costofown5_cel_all_recommended_costofownership5, 2), 'units' => 'value_usd'];
		}

		if ($total_costofown20_focl_all_recommended_costofownership20 != 0) {
			$summary[] = ['name' => 'total_costofown20_focl_all_recommended_costofownership20', 'value' => round($total_costofown20_focl_all_recommended_costofownership20, 2), 'units' => 'value_usd'];
		}

		if ($total_costofown20_mw_all_recommended_costofownership20 != 0) {
			$summary[] = ['name' => 'total_costofown20_mw_all_recommended_costofownership20', 'value' => round($total_costofown20_mw_all_recommended_costofownership20, 2), 'units' => 'value_usd'];
		}

		if ($total_costofown20_sat_all_recommended_costofownership20 != 0) {
			$summary[] = ['name' => 'total_costofown20_sat_all_recommended_costofownership20', 'value' => round($total_costofown20_sat_all_recommended_costofownership20, 2), 'units' => 'value_usd'];
		}

		if ($total_costofown20_cel_all_recommended_costofownership20 != 0) {
			$summary[] = ['name' => 'total_costofown20_cel_all_recommended_costofownership20', 'value' => round($total_costofown20_cel_all_recommended_costofownership20, 2), 'units' => 'value_usd'];
		}

		if ($total_income_all_recommended_costofownership5 != 0) {
			$summary[] = ['name' => 'total_income_all_recommended_costofownership5', 'value' => round($total_income_all_recommended_costofownership5, 2), 'units' => 'value_usdperyear'];
		}

		if ($total_income_focl_all_recommended_costofownership5 != 0) {
			$summary[] = ['name' => 'total_income_focl_all_recommended_costofownership5', 'value' => round($total_income_focl_all_recommended_costofownership5, 2), 'units' => 'value_usdperyear'];
		}

		if ($total_income_mw_all_recommended_costofownership5 != 0) {
			$summary[] = ['name' => 'total_income_mw_all_recommended_costofownership5', 'value' => round($total_income_mw_all_recommended_costofownership5, 2), 'units' => 'value_usdperyear'];
		}

		if ($total_income_all_recommended_costofownership20 != 0) {
			$summary[] = ['name' => 'total_income_all_recommended_costofownership20', 'value' => round($total_income_all_recommended_costofownership20, 2), 'units' => 'value_usdperyear'];
		}

		if ($total_income_focl_all_recommended_costofownership20 != 0) {
			$summary[] = ['name' => 'total_income_focl_all_recommended_costofownership20', 'value' => round($total_income_focl_all_recommended_costofownership20, 2), 'units' => 'value_usdperyear'];
		}

		if ($total_income_mw_all_recommended_costofownership20 != 0) {
			$summary[] = ['name' => 'total_income_mw_all_recommended_costofownership20', 'value' => round($total_income_mw_all_recommended_costofownership20, 2), 'units' => 'value_usdperyear'];
		}

		return $this->respondWithData(['table' => $summary, 'diagram' => empty($diagrams) ? ['nodiagrams'=>true] : $diagrams]);
	}

	public function action_topologyGraphData()
	{
		$formatMessage = function ($text) {
			return str_replace(["\n"], ["<br>"], $text);
		};

		set_time_limit(120);
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id'], true);

		if ((in_array($user->app_mode,['global','countries']) && $user->role !== UserModel::ROLE_SYSADMIN) && $project['user_id'] !== $user->id) {
			$this->deny();
		}

		$settings = json_decode($project['settings']);
		if ( (isset($settings->isSchoolsMode) && $settings->isSchoolsMode) && $settings->is_net_in_output !== true) {
			//Skip topology
			return $this->respondWithData([]);
		}

		//Process saved data
		$resultsModel = new ResultsModel($this->db);
		$topology = $resultsModel->getResultsByProjectId('topology', $project['id']);
		$input_data = json_decode($project['inputdata']);
		$paths = [];
		$markers = [];
		$markers_message = [];
		$paths_message = [];
		$maxbounds = [[90, 180], [-90, -180]];

		if (empty($topology)) {
			return $this->respondWithData([]);
		}

		$topology = json_decode(array_pop($topology)['data']);
		if (isset($topology->main)) {//v1 topology
			$topology_log = $resultsModel->getLogsByProjectId('topology', $project['id']);
			$topology_log = json_decode(ResultsModel::decompressData(array_pop($topology_log)['data']));

			if (!empty($topology->main) && !empty($topology->main[0]))
				foreach ($topology->main as $row_index => $row) {
					$lat = (float)$input_data->objects[$row_index]->{5};
					$lng = (float)$input_data->objects[$row_index]->{6};
					if ($lat < $maxbounds[0][0]) $maxbounds[0][0] = $lat;
					if ($lat > $maxbounds[1][0]) $maxbounds[1][0] = $lat;
					if ($lng < $maxbounds[0][1]) $maxbounds[0][1] = $lng;
					if ($lng > $maxbounds[1][1]) $maxbounds[1][1] = $lng;
					//$input_data->objects[$row_index]->{2}
					$markers['m' . $row_index] = [
						'lat' => $lat, 'lng' => $lng,
						'draggable' => false,
						'focus' => false,
						'label' => [
							'message' => $input_data->objects[$row_index]->{2},
							'options' => [
								'noHide' => true
							]
						],
					];
					foreach ($row as $col_index => $col) {
						if ($row_index === $col_index) {
							//Node
							$markers_message[$input_data->objects[$row_index]->{2}] = $formatMessage($topology_log[$row_index][$col_index] ?? '');
							//break;
						}
						if (!empty($col)) {
							if (strstr($col, 'Microwave')) {
								$link['color'] = 'Tomato';
							} else if (strstr($col, 'Fiber')) {
								$link['color'] = 'DodgerBlue';
							} else if (strstr($col, 'Cell')) {
								$link['color'] = 'MediumSeaGreen';
							} else if (strstr($col, 'Satellite')) {
								continue;
								$link['color'] = 'Orange';
							}
							$link = [
								'weight' => 4,
								'label' => ['message' =>
									'<b>' . $input_data->objects[$row_index]->{2} . ' <-> ' . $input_data->objects[$col_index]->{2} . '</b><br>' .
									$formatMessage($topology_log[$row_index][$col_index] ?? '')],
								'latlngs' => [
									['lat' => $input_data->objects[$col_index]->{5}, 'lng' => $input_data->objects[$col_index]->{6}],
									['lat' => $input_data->objects[$row_index]->{5}, 'lng' => $input_data->objects[$row_index]->{6}],
								]
							];

							$paths['p' . $col_index . '.' . $row_index] = $link;
						}
					}
				}
		} else {//v2 topology
			if (sizeof($topology->map_lines[0]) > 3000) {
				return $this->respondWithData([]);
			}
			//Markers
			$indexes = []; $index = 0;
			$addMarker = function ($lat,$lng,$message,$isBaseNode=false,$group=null)  use (&$markers, &$index, &$indexes, &$maxbounds){
				if ($lat < $maxbounds[0][0]) $maxbounds[0][0] = $lat;
				if ($lat > $maxbounds[1][0]) $maxbounds[1][0] = $lat;
				if ($lng < $maxbounds[0][1]) $maxbounds[0][1] = $lng;
				if ($lng > $maxbounds[1][1]) $maxbounds[1][1] = $lng;
				//$input_data->objects[$row_index]->{2}
				$markers['m' . $index] = [
					'lat' => $lat, 'lng' => $lng,
					'draggable' => false,
					'focus' => false,
					'label' => [
						'message' => $message,
						'options' => [
							'noHide' => false
						]
					],
				];
				if ($isBaseNode) {
					$markers['m' . $index]['type'] = 'home';
				}
				if ($group !== null) {
					//$markers['m' . $index]['group'] = $group;
				}
				$indexes[sha1($lat.$lng)] = $index;
				$index++;
			};

			for ($i=0,$len=sizeof($topology->map_markers[0]);$i<$len;$i++) {
				if (is_array($topology->map_markers[0][$i])) {
					//Clusters
					for ($j=0,$jlen=sizeof($topology->map_markers[0][$i]);$j<$jlen;$j++) {
						if (empty($topology->map_markers[0][$i+2][$j])) continue;
						$addMarker($topology->map_markers[0][$i+1][$j],$topology->map_markers[0][$i][$j],$topology->map_markers[0][$i+2][$j],false,$i);
					}
					$i+=2;
				} else {
					//Nodes
					$marker = $topology->map_markers[0][$i];
					if (empty($marker->popup)) continue;
					$addMarker((float)$marker->lat,(float)$marker->lng,$marker->popup);
				}
				//if (is_array())
			}

			//Base nodes
			for ($i=0,$len=sizeof($topology->map_awe_markers[0]);$i<$len;$i++) {
				if (is_array($topology->map_awe_markers[0][$i])) {
					//Clusters
					for ($j=0,$jlen=sizeof($topology->map_awe_markers[0][$i]);$j<$jlen;$j++) {
						if (empty($topology->map_awe_markers[0][$i+2][$j])) continue;
						$addMarker($topology->map_awe_markers[0][$i+1][$j],$topology->map_awe_markers[0][$i][$j],$topology->map_awe_markers[0][$i+2][$j],true,$i);
					}
					$i+=2;
				} else {
					//Nodes
					$marker = $topology->map_awe_markers[0][$i];
					if (empty($marker->popup)) continue;
					$addMarker((float)$marker->lat,(float)$marker->lng,$marker->popup, true);
				}
			}
			//Links
			$links = [];
			//[[["37.5877719", "37.5945855", "0.3751336", "0.3684432", "FOCL:100"], ["37.584569", "37.5945855", "0.3593546", "0.3684432", "FOCL:100"]]]
			$index = 0;
			foreach ($topology->map_lines[0] as $item) {
				$paths['p'.$index] = [
					'weight' => 4,
					'label' => ['message' => $item[4] ?? ''],
					'latlngs' => [
						['lat' => (float)$item[2], 'lng' => (float)$item[0]],
						['lat' => (float)$item[3], 'lng' => (float)$item[1]],
					]
				];
				if (strstr($item[4], 'RTS')) {
					$paths['p'.$index]['color'] = 'Tomato';
				} else if (strstr($item[4], 'FOCL')) {
					$paths['p'.$index]['color'] = 'DodgerBlue';
				} else if (strstr($item[4], 'CELL')) {
					$paths['p'.$index]['color'] = 'MediumSeaGreen';
				} else if (strstr($item[4], 'SAT')) {
					$paths['p'.$index]['color'] = 'Orange';
				}
				$index++;
			}
		}

		return $this->respondWithData([
			'markers' => $markers,
			'paths' => $paths,
			'maxbounds' => $maxbounds,
			'markers_message' => $markers_message
		]);

	}

	public function action_downloadCalculationGeoJSON()
	{
		set_time_limit(300);
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id'], true);

		if ((in_array($user->app_mode,['global','countries']) && $user->role !== UserModel::ROLE_SYSADMIN) && $project['user_id'] !== $user->id) {
			$this->deny();
		}

		$resultsModel = new ResultsModel($this->db);
		$topology = $resultsModel->getResultsByProjectId('topology', $project['id']);
		$topology = json_decode(array_pop($topology)['data']);
		$paths = [];
		$features = [];
		$markers_message = [];
		$paths_message = [];
		$maxbounds = [[90, 180], [-90, -180]];
		$indexes = []; $index = 0;
		$addMarker = function ($lat,$lng,$message,$isBaseNode=false,$group=null)  use (&$features, &$index, &$indexes, &$maxbounds){
			if ($lat < $maxbounds[0][0]) $maxbounds[0][0] = $lat;
			if ($lat > $maxbounds[1][0]) $maxbounds[1][0] = $lat;
			if ($lng < $maxbounds[0][1]) $maxbounds[0][1] = $lng;
			if ($lng > $maxbounds[1][1]) $maxbounds[1][1] = $lng;
			//$input_data->objects[$row_index]->{2}
			$features['m' . $index] = new Feature(new Point([$lng, $lat]),[
				'name' => str_replace(' &lt;br&gt;',',',htmlentities($message)),
				'marker-color' => $isBaseNode ? 'blue' : 'grey',
				'marker-symbol' => $isBaseNode ? 'star' : 'circle',
				'baseNode' => $isBaseNode
			]);
			if ($group !== null) {
				//$markers['m' . $index]['group'] = $group;
			}
			$indexes[sha1($lat.$lng)] = $index;
			$index++;
		};

		if (empty($topology->map_markers[0])) {
			throw new Exception("Error in underlying geo data, can't build GeoJSON.", 500);
		}

		for ($i=0,$len=sizeof($topology->map_markers[0]);$i<$len;$i++) {
			if (is_array($topology->map_markers[0][$i])) {
				//Clusters
				for ($j=0,$jlen=sizeof($topology->map_markers[0][$i]);$j<$jlen;$j++) {
					if (empty($topology->map_markers[0][$i+2][$j])) continue;
					$addMarker($topology->map_markers[0][$i+1][$j],$topology->map_markers[0][$i][$j],$topology->map_markers[0][$i+2][$j],false,$i);
				}
				$i+=2;
			} else {
				//Nodes
				$marker = $topology->map_markers[0][$i];
				if (empty($marker->popup)) continue;
				$addMarker((float)$marker->lat,(float)$marker->lng,$marker->popup);
			}
			//if (is_array())
		}

		//Base nodes
		for ($i=0,$len=sizeof($topology->map_awe_markers[0]);$i<$len;$i++) {
			if (is_array($topology->map_awe_markers[0][$i])) {
				//Clusters
				for ($j=0,$jlen=sizeof($topology->map_awe_markers[0][$i]);$j<$jlen;$j++) {
					if (empty($topology->map_awe_markers[0][$i+2][$j])) continue;
					$addMarker($topology->map_awe_markers[0][$i+1][$j],$topology->map_awe_markers[0][$i][$j],$topology->map_awe_markers[0][$i+2][$j],true,$i);
				}
				$i+=2;
			} else {
				//Nodes
				$marker = $topology->map_awe_markers[0][$i];
				if (empty($marker->popup)) continue;
				$addMarker((float)$marker->lat,(float)$marker->lng,$marker->popup, true);
			}
		}
		//Links
		$links = [];
		//[[["37.5877719", "37.5945855", "0.3751336", "0.3684432", "FOCL:100"], ["37.584569", "37.5945855", "0.3593546", "0.3684432", "FOCL:100"]]]
		$index = 0;
		foreach ($topology->map_lines[0] as $item) {
			if (strstr($item[4], 'RTS')) {
				$color = 'Tomato';
			} else if (strstr($item[4], 'FOCL')) {
				$color = 'DodgerBlue';
			} else if (strstr($item[4], 'CELL')) {
				$color = 'MediumSeaGreen';
			} else if (strstr($item[4], 'SAT')) {
				$color = 'Orange';
			} else {
				$color = 'grey';
			}
			$features['p'.$index] = new Feature(new LineString([new Point([(float)$item[0],(float)$item[2]]), new Point([(float)$item[1],(float)$item[3]])]), [
				'stroke' => $color,
				'value' => $item[4]
			]);

			$index++;
		}

		$features = new FeatureCollection($features);
		$saved_filename = tempnam('/tmp', 'log' . $this->args['id']);
		file_put_contents($saved_filename, json_encode($features));
		//Server template to end user
		$fh = fopen($saved_filename, 'r');
		fseek($fh, 0);
		$stream = new Stream($fh);
		$filename = substr(ResultsModel::stripFilenameCharacters($project['name']),0,200) . '(geojson).json';
		session_write_close();
		return $this->response->withHeader('Content-Type', 'application/force-download')
			->withHeader('Content-Type', 'application/octet-stream')
			->withHeader('Content-Type', 'application/download')
			->withHeader('Content-Description', 'File Transfer')
			->withHeader('Content-Transfer-Encoding', 'binary')
			->withHeader('Content-Disposition', 'attachment; filename="' . $filename . '"')
			->withHeader('Expires', '0')
			->withHeader('Cache-Control', 'must-revalidate, post-check=0, pre-check=0')
			->withHeader('Pragma', 'public')
			->withBody($stream);
	}

	public function action_downloadCalculationLog()
	{
		set_time_limit(300);
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id'], true);

		if ((in_array($user->app_mode,['global','countries']) && $user->role !== UserModel::ROLE_SYSADMIN) && $project['user_id'] !== $user->id) {
			$this->deny();
		}

		$resultsModel = new ResultsModel($this->db);
		//Process saved data
		$input_data = json_decode($project['inputdata']);
		$objects_log = $resultsModel->getLogsByProjectId('object', $project['id']);
		//Save the results to a temporary file
		$saved_filename = tempnam('/tmp', 'log' . $this->args['id']);
		$log = $data = '';
		if (isset($objects_log[0])) {
			$data = '//' . $objects_log[0]['created'] . "\n\r\n\r";
			foreach ($objects_log as $obj) {
				$name = $input_data->objects[$obj['id']]->{2};
				$log = ResultsModel::decompressData($obj['data']);
				$data .= "---\n{$name}\n---\n" . $log . "\n\r\n\r";
			}
		}
		$topology_log = $resultsModel->getLogsByProjectId('topology', $project['id']);
		if (!empty($topology_log[0])) {
			$tlog = ResultsModel::decompressData($topology_log[0]['data']);
			if (!empty($tlog)) {
				$tlog = json_decode($tlog);
				foreach ($tlog as $tl) {
					$log .= is_array($tl) ? join("\n", $tl) : $tl . "\n";
				}
				$data .= "\n\r" . $log . "\n\r\n\r";
			}
		}
		$data .= '//' . (!empty($topology_log[0]['created']) ? $topology_log[0]['created'] : $objects_log[sizeof($objects_log)-1]['created']);
		file_put_contents($saved_filename, $data);

		//Server template to end user
		$fh = fopen($saved_filename, 'r');
		fseek($fh, 0);
		$stream = new Stream($fh);
		$filename = substr(ResultsModel::stripFilenameCharacters($project['name']),0,200) . '(log).txt';
		session_write_close();
		return $this->response->withHeader('Content-Type', 'application/force-download')
			->withHeader('Content-Type', 'application/octet-stream')
			->withHeader('Content-Type', 'application/download')
			->withHeader('Content-Description', 'File Transfer')
			->withHeader('Content-Transfer-Encoding', 'binary')
			->withHeader('Content-Disposition', 'attachment; filename="' . $filename . '"')
			->withHeader('Expires', '0')
			->withHeader('Cache-Control', 'must-revalidate, post-check=0, pre-check=0')
			->withHeader('Pragma', 'public')
			->withBody($stream);
	}

	public function action_downloadSmartReportByProjectIdAPI()
	{
		set_time_limit(300);
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id'], true);

		if ((in_array($user->app_mode,['global','countries']) && $user->role !== UserModel::ROLE_SYSADMIN) && $project['user_id'] !== $user->id) {
			$this->deny();
		}

		$resultsModel = new ResultsModel($this->db);
		$reports = $resultsModel->getReportsByProjectId('smart', $project['id']);
		if (empty($reports)) {
			//Generate report
			$input_data = json_decode($projectModel->getInputDataByProjectID($project['id']));
			$saved_filename = $projectModel->generateSmartReport($project['id'], $input_data, true);
		} else {
			$saved_filename = tempnam('/tmp', 'smartreport');
			$data = ResultsModel::decompressData(array_pop($reports)['data']);
			file_put_contents($saved_filename, $data);
		}

		//Serve template to end user
		$fh = fopen($saved_filename, 'r');
		fseek($fh, 0);
		$stream = new Stream($fh);
		$filename = substr(ResultsModel::stripFilenameCharacters($project['name']), 0, 200) . '(smart report).xlsx';
		session_write_close();
		return $this->response->withHeader('Content-Type', 'application/force-download')
			->withHeader('Content-Type', 'application/octet-stream')
			->withHeader('Content-Type', 'application/download')
			->withHeader('Content-Description', 'File Transfer')
			->withHeader('Content-Transfer-Encoding', 'binary')
			->withHeader('Content-Disposition', 'attachment; filename="' . $filename . '"')
			->withHeader('Expires', '0')
			->withHeader('Cache-Control', 'must-revalidate, post-check=0, pre-check=0')
			->withHeader('Pragma', 'public')
			->withBody($stream);
	}

	public function action_downloadReportByProjectIdAPI()
	{
		set_time_limit(300);
		$user = $this->authorize();

		$projectModel = new ProjectModel($this->db);
		$project = $projectModel->getProjectById($this->args['id'], true);

		if ((in_array($user->app_mode,['global','countries']) && $user->role !== UserModel::ROLE_SYSADMIN) && $project['user_id'] !== $user->id) {
			$this->deny();
		}

		$resultsModel = new ResultsModel($this->db);
		$reports = $resultsModel->getReportsByProjectId('smart', $project['id']);
		if (empty($reports)) {
			//Generate report
			$input_data = json_decode($projectModel->getInputDataByProjectID($project['id']));
			$saved_filename = $projectModel->generateSimpleReport($project['id'], $input_data, true);
		} else {
			$saved_filename = tempnam('/tmp', 'simplereport');
			$data = ResultsModel::decompressData(array_pop($reports)['data']);
			file_put_contents($saved_filename, $data);
		}

		//Server template to end user
		if (!isset($stream)) {
			$fh = fopen($saved_filename, 'r');
			fseek($fh, 0);
			$stream = new Stream($fh);
		}
		$filename = substr(ResultsModel::stripFilenameCharacters($project['name']),0,200) . '(report).xlsx';
		session_write_close();
		return $this->response->withHeader('Content-Type', 'application/force-download')
			->withHeader('Content-Type', 'application/octet-stream')
			->withHeader('Content-Type', 'application/download')
			->withHeader('Content-Description', 'File Transfer')
			->withHeader('Content-Transfer-Encoding', 'binary')
			->withHeader('Content-Disposition', 'attachment; filename="' . $filename . '"')
			->withHeader('Expires', '0')
			->withHeader('Cache-Control', 'must-revalidate, post-check=0, pre-check=0')
			->withHeader('Pragma', 'public')
			->withBody($stream);
	}

	public function getResultsByProjectIdAPI(\Slim\Http\Request $request, \Slim\Http\Response $response, $args)
	{

		if ($request->getAttribute('user')) {
			$current_user = $request->getAttribute('user');

			$projectModel = new ProjectModel($this->container);
			$project = $projectModel->getProjectById($args['id']);

			if ($current_user->role !== UserModel::ROLE_SYSADMIN && $current_user->id === $project['user_id']) {

				$objectModel = new ObjectModel($this->container);
				$objectsList = $objectModel->getObjectsListByProjectId($args['id'])['rows'];

				$resultsModel = new ResultsModel($this->container);
				$techModel = new TechnologyModel($this->container);

				$results = array();
				foreach ($objectsList as $key => $value) {
					$res = $resultsModel->getResultsByObjectId($value['id']);
					$object_results = [];
					foreach ($res as $result) {
						$object_results[] = array(
							'result_id' => $result['id'],
							'object_name' => $value['name'],
							'technology_name' => $techModel->getTechnologyById($result['technologies_id'])['name'],
							'npv' => $result['npv']
						);
					}
					usort($object_results, function ($a, $b) {
						if ($a['npv'] == $b['npv']) {
							return 0;
						}
						return ($a['npv'] > $b['npv']) ? -1 : 1;
					});
					$results = array_merge($results, $object_results);
				}

				return $response->withJson($results);

			} else {
				throw new Exception("Access denied.", 500);
			}

		} else {
			throw new Exception("Access denied.", 500);
		}

	}

	/**
	 * @param \Slim\Http\Request $request
	 * @param \Slim\Http\Response $response
	 * @return \Slim\Http\Response
	 */
	public function getResultsByObjectIdAPI(\Slim\Http\Request $request, \Slim\Http\Response $response, $args)
	{

		if ($request->getAttribute('user')) {
			$current_user = $request->getAttribute('user');

			$objectModel = new ObjectModel($this->container);
			$object = $objectModel->getObjectById($args['id'], $current_user->id);

			if (empty($object)) {
				return $response->withJson('');
			}

			$resultsModel = new ResultsModel($this->container);

			$results = array();
			$res = $resultsModel->getResultsByObjectId($object['id']);
			foreach ($res as $result) {
				$results[] = array(
					'result_id' => $result['id'],
					'technology_name' => $result['technology_name'],
					'npv' => $result['npv']
				);
			}

			return $response->withJson($results);

		} else {
			throw new Exception("Access denied.", 500);
		}

	}

	/**
	 * @param \Slim\Http\Request $request
	 * @param \Slim\Http\Response $response
	 * @return \Slim\Http\Response
	 */
	public function getResultsByObjectIdAndTechnologyIdAPI(\Slim\Http\Request $request, \Slim\Http\Response $response, $args)
	{

		if ($request->getAttribute('user')) {
			$current_user = $request->getAttribute('user');

			$objectModel = new ObjectModel($this->container);
			$object = $objectModel->getObjectById($args['object_id']);


			if ($current_user->id === $object['user_id']) {

				$techModel = new TechnologyModel($this->container);

				$results = array();

				$result = $objectModel->getResultByObjectIdAndTechnologiesId($args['object_id'], $args['technology_id']);

				$results[] = array(
					'result_id' => $result['id'],
					'object_name' => $object['name'],
					'technology_name' => $techModel->getTechnologyById($result['technologies_id'])['name'],
					'npv' => $result['npv']
				);

				return $response->withJson($results);

			} else {
				throw new Exception("Access denied.", 500);
			}

		} else {
			throw new Exception("Access denied.", 500);
		}

	}

}
