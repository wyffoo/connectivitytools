<?php

namespace App\Console;

use App\Models\ProjectModel;
use Matrix\Exception;
use PDO;
use Psr\Container\ContainerInterface;
use PhpAmqpLib\Connection\AMQPStreamConnection;
use PhpAmqpLib\Message\AMQPMessage;
use Psr\Log\LoggerInterface;

/**
 * Command.
 */
final class CalculateQueueCommand {

	/**
	 * @var ContainerInterface
	 */
	private $container;

	private $projectModel;

	const BATCH_SIZE = 4;

	/**
	 * Constructor.
	 *
	 * @param ContainerInterface $container The container
	 * @param string|null $name The name
	 */
	public function __construct(ContainerInterface $container, $name, $input) {
		$this->container = $container;
	}

	/**
	 * Execute command.
	 *
	 * @return int The error code, 0 on success
	 */
	public function execute(): int {
		$callback = function($msg) {
			$result = $this->process($msg);
			if (!is_string($msg) && $result) $msg->ack();
			return $result;
		};

		$connection = $this->container->get('rabbitmq');

		if ($connection === null) {
			//Running in AWS
			try {
				if ($callback($_ENV['CALC_MESSAGE_BODY']) === true) {
					return 0;
				} else {
					return 1;
				}
			} catch (Exception $e) {
				var_dump($e->getMessage());
				return 1;
			}
		} else {
			echo " [*] Waiting for messages. To exit press CTRL+C\n";

			$queue_name = 'giga_calculation_requests_old';
			$channel = $connection->channel();

			$channel->queue_declare($queue_name, false, true, false, false);

			$channel->basic_qos(null, 1, null);

			$channel->basic_consume('giga_calculation_requests_old', '', false, false, false, false, $callback);
			while ($channel->is_consuming()) {
				$channel->wait();
			}

			$channel->close();
			$connection->close();
		}
	}

	private function getProjectModelInstance() {
		$settings = $this->container->get('settings')['db'];
		$pdo = new \PDO("mysql:host=" . $settings['host'] . ";port=" . $settings['port'] . ";dbname=" . $settings['dbname'] . ';charset=utf8',
			$settings['user'], $settings['pass']);
		$pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
		$pdo->setAttribute(PDO::ATTR_DEFAULT_FETCH_MODE, PDO::FETCH_ASSOC);
		$this->projectModel = new ProjectModel($pdo, $this->container->get('settings')['dirs']);
		return $this->projectModel;
	}

	private function parseRScriptOutput($r_output) {
		$raw_results = explode("\n", $r_output);
		$results = '';
		$log = '';
		$status = 0;
		$object_id = 0;
		$read_status = $read_result = $read_log = $read_object_id = false;
		for ($ir = 0, $irs = sizeof($raw_results); $ir < $irs; $ir++) {
			if ($raw_results[$ir] == '--START:OBJECT_ID--') {
				$read_object_id = true;
			} elseif ($raw_results[$ir] == '--END:OBJECT_ID--') {
				$read_object_id = false;
			} elseif ($raw_results[$ir] == '--START:RESULTS--') {
				$read_result = true;
			} elseif ($raw_results[$ir] == '--END:RESULTS--') {
				$read_result = false;
			} elseif ($raw_results[$ir] == '--START:LOG--') {
				$read_log = true;
			} elseif ($raw_results[$ir] == '--END:LOG--') {
				$read_log = false;
			} elseif ($raw_results[$ir] == '--START:STATUS--') {
				$read_status = true;
			} elseif ($raw_results[$ir] == '--END:STATUS--') {
				$read_status = false;
			} elseif ($read_log === true) {
				$log .= $raw_results[$ir] . "\n";
			} elseif ($read_result === true) {
				$results = $raw_results[$ir];
			} elseif ($read_status === true) {
				$status = (int)$raw_results[$ir];
			} elseif ($read_object_id === true) {
				$object_id = (int)$raw_results[$ir];
			}
		}
		return [$object_id, $status, $results, $log];
	}

	public function process($msg) {
		if (isset($_ENV['CALC_MESSAGE_BODY'])) {//aws sqs
			$message = json_decode($msg);
		} else {
			$message = json_decode($msg->body);
		}
		$projectModel = $this->getProjectModelInstance();
		$abort_calculation = function ($throwException = false) use ($message) {
			echo date('r', time()) . " [x] Project calculation cancellation was requested, aborting and removing from the queue, Project ID {$message->project_id}, Mode: {$message->mode}\n";
			if ($throwException) {
				throw new \Exception('Project calculation cancellation was requested, aborting and removing from the queue');
			} else {
				return true;
			}
		};
		echo date('r', time()) . " [x] Received calculation request for Project ID {$message->project_id}, Mode: {$message->mode}.\n Calculation will start in 5 seconds.\n";
		sleep(5);//Wait for db updates
		$project = $projectModel->getProjectById($message->project_id);

		if ($project['status'] === 'stop') {
			$projectModel->updateProjectStatus($message->project_id, 'not-calculated');
			return $abort_calculation(false);
		}
		if ($project['status'] === 'in-progress') {
//			echo date('r', time()) . " [x] Project is in calculation on a different container, aborting and removing from the queue, Project ID {$message->project_id}, Mode: {$message->mode}\n";
//			$msg->ack();
//			return;
		}
		if ($project['status'] === 'calculated' || $project['status'] === 'partially-calculated') {
			echo date('r', time()) . " [x] Project was calculated earlier, aborting and removing from the queue, Project ID {$message->project_id}, Mode: {$message->mode}\n";
			return true;
		}
		$input_data = json_decode($projectModel->getInputDataByProjectID($message->project_id));
		if (empty($input_data)) {
			echo date('r', time()) . " [x] No input data or project was deleted, aborting and removing from the queue, Project ID {$message->project_id}, Mode: {$message->mode}\n";
			return true;
		}

		$export_class_name = $project['mode'] === 'schools' ? $input_data->output_template->class : 'GlobalOutputTemplate';
		$percent = 0;
		try {
			$projectModel->updateProjectStatus($message->project_id, 'in-progress', 1);
			chdir('./InvestigationTool');

			$i = 0;
			$isize = sizeof($input_data->objects);
			$results = [
				'topology' => [],
				'objects' => [],
				'ts' => ['start' => time(), 'end' => null]
			];
			$status = 'calculated';
			$status_data = null;
			$run_id = uniqid();

			//Cycle through every object
			$per_percent = 100 / sizeof($input_data->objects);

			$progress = $projectModel->getProjectProgressById($message->project_id);
			if ($progress['status'] === 'stop') {
				$abort_calculation();
				return;
			}

			//Prepare coordinates file
			if ($project['mode'] === 'schools') {
				$coordinates_file = $this->prepareCoordinatesSchools($input_data, $input_data->calculation_parameters->is_bandwidth_calc_required === 'provided', true);
			} else {
				$coordinates_file = $this->prepareCoordinatesCountries($input_data, true, true);
			}

			//Calculate distance to fiber
			$variable_file = $this->prepareVariables(null, $input_data, $export_class_name);
			var_dump('Rscript --default-packages="methods,dplyr,shiny,readr" --vanilla giga.R 0 dtf ' . $variable_file . ' ' . $coordinates_file . " #PROJECTID:{$message->project_id}");
			$r_output = shell_exec('Rscript --default-packages="methods,dplyr,shiny,readr" --vanilla giga.R 0 dtf ' . $variable_file . ' ' . $coordinates_file . " #PROJECTID:{$message->project_id}");
			list($object_id, $r_status, $dtf_results, $log) = $this->parseRScriptOutput($r_output);
			$dtf_results = json_decode($dtf_results);
			$dtf_log = [];

			if (!isset($dtf_results->status) && !empty($dtf_results->main) && sizeof($dtf_results->main) > 0) {
				foreach ($dtf_results->main as $row) {
					$dtf_object_id = (int)$row[0];
					$dtf_value = (float)$row[1];
					$dtf_closest_object_id = (int)$row[2];
					if ($dtf_value > 0) {
						if ($project['mode'] === 'schools') {
							$input_data->objects[$dtf_object_id]->{9} = $dtf_value;//Add calculated DTF to the object
							$dtf_log[$dtf_object_id] = 'Closest object with fiber: ' . $input_data->objects[$dtf_closest_object_id]->{2};
						} else {
							$input_data->objects[$dtf_object_id]->{15} = $dtf_value;
							$dtf_log[$dtf_object_id] = 'Closest object with fiber: ' . $input_data->objects[$dtf_closest_object_id]->{1};
						}

					}
				}
			}

			if (($project['mode'] === 'schools' || (in_array($project['mode'],['global','countries']) && $input_data->data_type==='city')) && $message->mode === 'complete') {
				$batch_size = 0;
				$dispatcher = new \FastBill\ParallelProcessDispatcher\Dispatcher(self::BATCH_SIZE);
				foreach ($input_data->objects as $object) {
					if (!$input_data->objects_validity[$i]) {
						$status = 'partially-calculated';
						continue;//Skip object calculations
					}
					if ((int)($per_percent * $i) % 10 === 0) {
						$projectModel = $this->getProjectModelInstance();
						$progress = $projectModel->getProjectProgressById($message->project_id);
						if ($progress['status'] === 'stop') {
							$abort_calculation();
						}
						$projectModel->updateProjectStatus($message->project_id, 'in-progress', round($per_percent * $i, 1));
					}
					$variable_file = $this->prepareVariables($object, $input_data, $export_class_name);

					//Traffic
					$traffic_file = 'null';
					$break_calculation = false;
					if (in_array($export_class_name, ['SimpleBBCalcOnly', 'ExtendedBBCalcOnly', 'SimpleBBCalcLAN', 'ExtendedBBCalcLAN'])) {
						$traffic_file = $this->prepareTraffic($input_data->traffic_matrix[$i]);
					}
					if ($batch_size < self::BATCH_SIZE) {
						$process = new \FastBill\ParallelProcessDispatcher\Process('Rscript --default-packages="methods,dplyr,shiny,readr" --vanilla giga.R '
							. $i . ' ' . $export_class_name . ' ' . $variable_file . ' ' . $traffic_file . " #PROJECTID:{$message->project_id}");
						var_dump('Rscript --default-packages="methods,dplyr,shiny,readr" --vanilla giga.R ' . $i . ' ' . $export_class_name . ' ' . $variable_file . ' ' . $traffic_file . " #PROJECTID:{$message->project_id}");
						$dispatcher->addProcess($process);
						$batch_size++;
					}
					if ($batch_size === self::BATCH_SIZE || $i == ($isize - 1)) {
						$batch_size = 0;
						$dispatcher->dispatch();  // this will run until all processes are finished.
						$processes = $dispatcher->getFinishedProcesses();
						foreach ($processes as $process) {
							$r_output = $process->getOutput();
							//var_dump($r_output);
							list($object_id, $r_status, $results, $log) = $this->parseRScriptOutput($r_output);

							if (empty($results)) {
								if ($msg->isRedelivered()) {
									$msg->ack();
								}
								if ($per_percent * $object_id > 50) {
									$status = 'partially-calculated';
									$break_calculation = true;
									break;
								} else {
									throw new Exception("Wrong results: " . var_export($r_output, true));
								}
							}
							if ($input_data->calculation_parameters->is_bandwidth_calc_required !== 'provided') {
								//Collect bandwidth for topology
								//"required_bandwidth":[219.25]
								preg_match_all('/"required_bandwidth":\[([0-9\.]+)\]/', $results, $matches);
								$input_data->objects[$object_id]->bandwidth = (float)$matches[1][0];
							}
							$projectModel = $this->getProjectModelInstance();
							$results = preg_replace('/"required_bandwidth":\[([\d\.]+)\]/', '"required_bandwidth":$1', $results);
							if (isset($dtf_log[$object_id])) {
								$results = str_replace('"main":[', '"calculated_dtf":' . $input_data->objects[$dtf_object_id]->{9} . ',"main":[', $results);
								$log .= $dtf_log[$object_id];
							}
							$projectModel->saveResult($run_id, $message->project_id, $results, 'object', $object_id);
							$projectModel->saveResult($run_id, $message->project_id, $log, 'object-log', $object_id);
						}
						$dispatcher = new \FastBill\ParallelProcessDispatcher\Dispatcher(self::BATCH_SIZE);
					}
					if ($break_calculation) {
						break;
					}
					$i++;
				}
			}

			$progress = $projectModel->getProjectProgressById($message->project_id);
			if ($progress['status'] === 'stop') {
				return $abort_calculation(false);
			}

			//Calculate coordinates for topology and calculate topology ONCE
			if (in_array($project['mode'],['global','countries']) || (isset($input_data->calculation_parameters->is_net_in_output) && $input_data->calculation_parameters->is_net_in_output === true)) {
				if ($project['mode'] === 'schools') {
					$coordinates_file = $this->prepareCoordinatesSchools($input_data, $input_data->calculation_parameters->is_bandwidth_calc_required === 'provided');
				} else {
					$coordinates_file = $this->prepareCoordinatesCountries($input_data, true);
				}
				var_dump('Rscript --default-packages="methods,dplyr,shiny,readr,SDMTools" --vanilla giga.R 0 topology ' . $variable_file . ' ' . $coordinates_file);
				$r_output = shell_exec('Rscript --default-packages="methods,dplyr,shiny,readr,SDMTools" --vanilla giga.R 0 topology ' . $variable_file . ' ' . $coordinates_file . " #PROJECTID:{$message->project_id}");
				list($object_id, $r_status, $results, $log) = $this->parseRScriptOutput($r_output);
				if (empty($results) || $r_status >= 200) {
					$status = 'partially-calculated';
					$status_log = @json_decode($log);
					$status_data = '{"code":' . $r_status . ',"log":"' . @array_pop($status_log) . '"}';
				}
				$projectModel = $this->getProjectModelInstance();
				if ($message->mode === 'topology') {
					//Merge topology results with a previous full run (if there was a run)
					if ($previous_run_id = $projectModel->getLatestCompleteRunId($message->project_id)) {
						$run_id = $previous_run_id;
					}
				}
				if (!empty($results)) {
					$projectModel->saveResult($run_id, $message->project_id, $results, 'topology');
					$projectModel->saveResult($run_id, $message->project_id, $log, 'topology-log');
				}
			}

			chdir('..');

			$progress = $projectModel->getProjectProgressById($message->project_id);
			if ($progress['status'] === 'stop') {
				return $abort_calculation(false);
			}

			//Generate reports
			if ($project['mode'] === 'schools' && $message->mode === 'complete') {
				$filename = $projectModel->generateSimpleReport($message->project_id, $input_data);
				$projectModel = $this->getProjectModelInstance();
				$projectModel->saveResult($run_id, $message->project_id, file_get_contents($filename), 'report-simple');

				$filename = $projectModel->generateSmartReport($message->project_id, $input_data);
				$projectModel = $this->getProjectModelInstance();
				$projectModel->saveResult($run_id, $message->project_id, file_get_contents($filename), 'report-smart');
			}

			//Update project with results
			$projectModel = $this->getProjectModelInstance();
			$projectModel->updateProjectStatus($message->project_id, $status, 100, $status_data);
			return true;
		} catch (\Exception $e) {
			$projectModel = $this->getProjectModelInstance();
			$projectModel->updateProjectStatus($message->project_id, 'not-calculated');
			throw $e;
		}
	}

	private function prepareCoordinatesSchools($input_data, $is_bandwidth_provided, $dtf_mode = false) {
		$filename = tempnam('/tmp', 'coordinates');
		$file = fopen($filename, 'w+');

		#Первая колонка OBJECTNAME - какой-то идентификатор школы (хотя пока я его не юзаю толком)
		#Затем LON и LAT
		#Затем RB - пропускная способность - для этого шаблона она задаётся юзером
		#DTF - расстояние до волокна

		#TODO NEW - Add ONE COLOUMN AT THE END -> REGION...should be received from 3rd and 4th coloumns of the input template (Region & Subregion)
		fputcsv($file, ['OBJECTNAME', 'LON', 'LAT', 'RB', 'DTF', 'COV', 'REGION'], ';');
		$i = 0;
		foreach ($input_data->objects as $object) {
			if (!$input_data->objects_validity[$i]) {
				continue;//Skip object calculations
			}
			fputcsv($file, [
				$dtf_mode ? $i : $object->{1} . '-' . $object->{2},
				$object->{6},
				$object->{5},
				$is_bandwidth_provided ? $object->{10} : $object->bandwidth ?? 0,//Bandwidth or calculated bandwidth from results
				$object->{9},
				$object->{7},
				$object->{3} . '-' . $object->{4},
			], ';');
			$i++;
		}
		fclose($file);

		return $filename;
	}

	private function prepareCoordinatesCountries($input_data, $is_bandwidth_provided, $dtf_mode = false) {
		$filename = tempnam('/tmp', 'coordinates');
		$file = fopen($filename, 'w+');

		#Первая колонка OBJECTNAME - какой-то идентификатор школы (хотя пока я его не юзаю толком)
		#Затем LON и LAT
		#Затем RB - пропускная способность - для этого шаблона она задаётся юзером
		#DTF - расстояние до волокна

		#TODO NEW - Add ONE COLOUMN AT THE END -> REGION...should be received from 3rd and 4th coloumns of the input template (Region & Subregion)
		if ($input_data->data_type === 'global_ext') {
			fputcsv($file, ['OBJECTNAME', 'LON', 'LAT', 'RB', 'DTF', 'COV', 'REGION', 'POC','POCLON','POCLAT'], ';');
		} else {
			fputcsv($file, ['OBJECTNAME', 'LON', 'LAT', 'RB', 'DTF', 'COV', 'REGION'], ';');
		}
		$i = 0;
		foreach ($input_data->objects as $object) {
			//die(var_dump($object));
			if (!$input_data->objects_validity[$i]) {
				continue;//Skip object calculations
			}
			switch ($input_data->data_type) {
				case 'global':
					//OBJECTNAME;LON;LAT;RB;DTF;COV;REGION;POC;POCLON;POCLAT
					fputcsv($file, [
						$dtf_mode ? $i : (string)$object->{1},
						$object->{2},
						$object->{3},
						$object->{4},
						$object->{5},
						$object->{6},
						$object->{7}
					], ';');
					break;
				case 'global_ext':
					//OBJECTNAME;LON;LAT;RB;DTF;COV;REGION;POC;POCLON;POCLAT
					fputcsv($file, [
						$dtf_mode ? $i : (string)$object->{1},
						$object->{2},
						$object->{3},
						$object->{4},
						$object->{5},
						$object->{6},
						$object->{7},
						$object->{8},
						$object->{9},
						$object->{10},
					], ';');
					break;
				case 'hexagon':
					//`identifier`,`lat`,`long`,`population`,`type`,`parent_identifier`,`rb`,`flag`
					fputcsv($file, [
						$dtf_mode ? $i : (string)$object->{1},
						$object->{3},
						$object->{2},
						$object->{7},
						$object->{15} ?? ($object->{8} === 'cn' ? 0 : ''),
						'',
						'',
					], ';');
					break;
				case 'city':
					//'identifier','name','alt_name','lat','long','admin_name','admin_code','population','flag','area'
					fputcsv($file, [
						$dtf_mode ? $i : $object->{1}.' ',
						$object->{5},
						$object->{4},
						'2',//TODO: what field?
						$object->{15} ?? '',
						'',
						'',
					], ';');
					break;
				case 'social_point':
					fputcsv($file, [
						$dtf_mode ? $i : (string)$object->{1},
						$object->{2},
						$object->{3},
						$object->{4},
						$object->{5},
						$object->{6},
						$object->{7},
					], ';');
					break;
			}
			$i++;
		}
		fclose($file);

		return $filename;
	}

	private function prepareTraffic($traffic) {
		$filename = tempnam('/tmp', 'traffic');
		$file = fopen($filename, 'w+');
		fputcsv($file, ['DEVICENUMBER', 'BITRATE', 'INTENCITY', 'DATAVOLUME'], ';');
		foreach ($traffic as $line) {
			fputcsv($file, $line, ';');
		}
		fclose($file);
		return $filename;
	}

	private function prepareVariables($object, $input_data, $export_class_name) {
		$shared_vars = $input_data->shared_vars;
		$template_vars = $input_data->template_vars;
		$calculation_parameters = $input_data->calculation_parameters;
		$vars = [];
		foreach ($shared_vars as $var) {
			$vars[trim($var->name)] = (float)$var->value;
		}
		if ($object !== null) {
			foreach ($template_vars as $var) {
				$vars[trim($var->name)] = (float)$object->{$var->templates_col_number};
			}
		}

		//Broadband connection between schools
		if (isset($calculation_parameters->net_in_output_optimization)) {
			$vars['InitialData.NetInOutputOptimization'] = $calculation_parameters->net_in_output_optimization;
		} else {
			$vars['InitialData.NetInOutputOptimization'] = 'npv';
		}

		if (isset($calculation_parameters->net_in_output_techs)) {
			$vars['InitialData.UseFOCL'] = in_array('focl', $calculation_parameters->net_in_output_techs) ? 1 : 0;
			$vars['InitialData.UseRTS'] = in_array('rts', $calculation_parameters->net_in_output_techs) ? 1 : 0;
			$vars['InitialData.UseSatellite'] = in_array('satellite', $calculation_parameters->net_in_output_techs) ? 1 : 0;
			$vars['InitialData.UseCellular'] = in_array('cellular', $calculation_parameters->net_in_output_techs) ? 1 : 0;
		}

		//Electricity
		if ($object !== null) {
			if (in_array($export_class_name, ['BBRBOnly', 'ExtendedBBCalcLAN', 'ExtendedBBCalcOnly', 'ExtendedBBRBLAN', 'SimpleBBCalcLAN', 'SimpleBBCalcOnly', 'SimpleBBRBLAN'])
				&& trim($object->{"8"}) === 'Yes') {
				$vars['InitialData.CalculateElectricity'] = '0';
			} else {
				$vars['InitialData.CalculateElectricity'] = '1';
			}
		}

		//Level of quality
		if (isset($calculation_parameters->is_bandwidth_calc_required) && $calculation_parameters->is_bandwidth_calc_required !== 'provided') {
			switch ($input_data->traffic_profile->quality_level) {
				case 'LOW':
					$vars['InitialDataTraffic.LevelOfQuality'] = 0.05;
					break;
				case 'MEDIUM':
					$vars['InitialDataTraffic.LevelOfQuality'] = 0.005;
					break;
				case 'HIGH':
					$vars['InitialDataTraffic.LevelOfQuality'] = 0.0005;
					break;
			}
		}

		if ($object !== null && $input_data->data_type === 'city') {
			$vars['GeographicalParametersSet.Square'] = (float)$object->{'10'}; //area
			$vars['PopulationOptionsSet.Population'] = (float)$object->{'8'}; //population
			$vars['AccessTechnologyOptionsSet.MediumTypeOfSL'] = '1';
			//$vars['AccessTechnologyOptionsSet.QuantityFloorsEOA'] = 1;
			$vars['Intermediate.BuiltupArea'] = 1000;
			$vars['DevelopmentParametersSet.AverageFloorsLowrise'] =1;
			$vars['DevelopmentParametersSet.LowriseMultifamily'] = 0;
			$vars['AccessLevelOptionsSet.DemandTourist'] = 20;
			$vars['Intermediate.BaseNumberOfPopulation'] = 10000;
			$vars['Intermediate.BaseSubscribers'] = 10000;
			$vars['Intermediate.Subscribers'] = 10000;
			//AccessTechnologyOptionsSet.QuantityFloorsEOA
			//AccessLevelOptionsSet.DemandTourist
			//Intermediate.BaseNumberOfPopulation
			//Intermediate.BaseSubscribers
			//Intermediate.Subscribers
		}

		$vars['CountriesDataType'] = $input_data->data_type ?? 'none';
		if (isset($input_data->calculation_parameters->isGlobalMode) ) {
			//Enable TopologySettings.UseMetric
			$vars['TopologySettings.UseMetric'] = 1;
		}

		$filename = tempnam('/tmp', 'variables');
		$file = fopen($filename, 'w+');
		fputcsv($file, array_keys($vars), ';');
		fputcsv($file, array_values($vars), ';');
		fclose($file);
		return $filename;
	}
}
