<?php
declare(strict_types=1);

use DI\ContainerBuilder;
use Monolog\Logger;

return function (ContainerBuilder $containerBuilder) {
	// Global Settings Object
	$base = [
		'settings' => [
			'compressProjectBlobs' => true, // To save DB space we can compress input and results data
			'displayErrorDetails' => true, // Should be set to false in production
			'dirs' => [
				'reports' => '/opt/app_data/reports',
				'results' => '/opt/app_data/results'
			],
			'commands' => [
				'RunCalculationWorker' => App\Console\CalculateQueueCommand::class,
				'ImportLocations' => App\Console\ImportLocationsCommand::class
			],
			'logger' => [
				'name' => 'slim-app',
				'path' => isset($_ENV['docker']) ? 'php://stdout' : __DIR__ . '/../logs/app.log',
				'level' => Logger::DEBUG,
			],
			// Database connection settings
			'db' => [
				'host' => '192.168.5.36',
				'port' => '32773',
				'dbname' => 'giga',
				'user' => 'root',
				'pass' => 'LbceQM0NE6ZuhW57MExFl6N6b32BCvbG'
			],
			'sqs' => [
				'region' => 'eu-central-1',
				'url' => 'https://sqs.eu-central-1.amazonaws.com/238974323615/giga-calc-requests.fifo'
			],
			'rabbitmq' => [
				'host' => '192.168.5.36',
				'port' => 5672,
				'user' => 'guest',
				'pass' => 'guest'
			],
			'recaptcha' => [
				'secret' => '6LcQ3FIaAAAAABEc8HBiaJWvKk8IO6wuo0E7Q_X1'
			],

			'mail' => [
				'mailFrom' => 'admin@domain.com',
				'nameFrom' => 'Admin',

				'mailReplyTo' => 'no-reply@domain.com',
				'nameReplyTo' => 'No-Reply'
			],
		],
	];

	$prod = [
		'compressProjectBlobs' => true, // To dave DB space we can compress input and results data
		'displayErrorDetails' => false, // Should be set to false in production
		'db' => [
			'host' => '127.0.0.1',
			'port' => '3306',
			'dbname' => 'giga',
			'user' => 'giga',
			'pass' => 'LbceQM0NE6ZuhW57MExFl6N6b32BCvbG'
		],
		'rabbitmq' => [
			'host' => '192.168.5.36',
			'port' => 5672,
			'user' => 'guest',
			'pass' => 'guest'
		],
	];

	$settings = $base;

	if (isset($_ENV['ENV']) && $_ENV['ENV'] === 'prod') {
		$settings['settings'] = array_merge($base['settings'], $prod);
	} else if (isset($_ENV['ENV']) && $_ENV['ENV'] === 'aws_prod') {
		$aws_prod = [
			'compressProjectBlobs' => true, // To dave DB space we can compress input and results data
			'displayErrorDetails' => false, // Should be set to false in production
			'db' => [
				'host' => $_ENV['MYSQL_HOST'],
				'port' => $_ENV['MYSQL_PORT'],
				'dbname' => 'giga',
				'user' => 'giga',
				'pass' => 'LbceQM0NE6ZuhW57MExFl6N6b32BCvbG'
			],
			'rabbitmq' => [
				'host' => $_ENV['RABBITMQ_HOST'],
				'port' => $_ENV['RABBITMQ_PORT'],
				'user' => 'btc',
				'pass' => 'ZOle8bj5MJg@rZPNg$kc'
			],
			'sqs' => [
				'region' => 'eu-central-1',
				'url' => 'https://sqs.eu-central-1.amazonaws.com/238974323615/giga-calc-requests.fifo'
			],

		];
		$settings['settings'] = array_merge($base['settings'], $aws_prod);
	} else {
		$_ENV['ENV'] = 'dev';
	}

	$containerBuilder->addDefinitions($settings);
};
