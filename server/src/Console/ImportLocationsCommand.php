<?php

namespace App\Console;

use App\Models\InputTemplates\GlobalInputTemplate;
use Psr\Container\ContainerInterface;

/**
 * Command.
 */
final class ImportLocationsCommand {
	/**
	 * @var ContainerInterface
	 */
	private $container;

	/**
	 * Constructor.
	 *
	 * @param ContainerInterface $container The container
	 * @param string|null $name The name
	 */
	public function __construct(ContainerInterface $container, $name, $input) {
		$this->container = $container;
		$this->input = $input;
		try {
			$definition = new \Symfony\Component\Console\Input\InputDefinition([
				new \Symfony\Component\Console\Input\InputArgument('name', \Symfony\Component\Console\Input\InputArgument::REQUIRED),
				new \Symfony\Component\Console\Input\InputOption('country', 'c', \Symfony\Component\Console\Input\InputOption::VALUE_REQUIRED),
				new \Symfony\Component\Console\Input\InputOption('filename', 'f', \Symfony\Component\Console\Input\InputOption::VALUE_REQUIRED),
			]);
			$this->input->bind($definition);
		} catch (\Exception $e) {
			die($e);
		}
	}

	/**
	 * Execute command.
	 *
	 * @return int The error code, 0 on success
	 */
	public function execute(): int {

		$options = $this->input->getOptions();

		$filename = realpath($options['filename']);

		//Check file
		if (!is_file($filename) || !is_readable($filename)) {
			echo "No such file: {$options['filename']}\n";
			return 2;
		}

		//Check country
		$db = $this->container->get('db');
		$getData = $db->prepare("SELECT id,country_name FROM country WHERE lower(country_name)=lower(:name)");
		$getData->bindParam(1, $options['country']);
		$getData->execute();
		$country = $getData->fetch();

		if (empty($country)) {
			echo "No such country in the database.\n";
			return 2;
		} else {
			echo "Found country: {$country['country_name']}\n";
		}

		//Parse template
		$template = new GlobalInputTemplate($filename);
		//Parse template
		echo "Processing records...\n";
		$locations = $template->parseCSVLocations();
		try {
			$country_id = $country['id'];
			foreach ($locations as $loc) {
				$ins = $db->prepare("INSERT IGNORE INTO locations (identifier,name,alt_name,lat,`long`,admin_name,admin_code,population,flag,area,country_id)
					VALUES (:identifier,:name,:alt_name,:lat,:long,:admin_name,:admin_code,:population,:flag,:area,:country_id)");
				$ins->bindParam(':identifier', $loc[1]);
				$ins->bindParam(':name', $loc[2]);
				$ins->bindParam(':alt_name', $loc[3]);
				$ins->bindParam(':lat', $loc[4]);
				$ins->bindParam(':long', $loc[5]);
				$ins->bindParam(':admin_name', $loc[6]);
				$ins->bindParam(':admin_code', $loc[7]);
				$ins->bindParam(':population', $loc[8]);
				$ins->bindParam(':flag', $loc[9]);
				$ins->bindParam(':area', $loc[10]);
				$ins->bindParam(':country_id', $country_id);
				$ins->execute();
			}
		} catch (\Exception $e) {
			die($e);
		}
		return 0;
	}


}
