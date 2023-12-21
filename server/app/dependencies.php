<?php
declare(strict_types=1);

use DI\ContainerBuilder;
use Monolog\Handler\StreamHandler;
use Monolog\Logger;
use Monolog\Processor\UidProcessor;
use PhpAmqpLib\Connection\AMQPStreamConnection;
use PhpAmqpLib\Connection\AMQPSSLConnection;
use Psr\Container\ContainerInterface;
use Psr\Log\LoggerInterface;
use Aws\Sqs\SqsClient;

return function (ContainerBuilder $containerBuilder) {
	$containerBuilder->addDefinitions([
		LoggerInterface::class => function (ContainerInterface $c) {
			$settings = $c->get('settings');

			$loggerSettings = $settings['logger'];
			$logger = new Logger($loggerSettings['name']);

			$processor = new UidProcessor();
			$logger->pushProcessor($processor);

			$handler = new StreamHandler($loggerSettings['path'], $loggerSettings['level']);
			$logger->pushHandler($handler);

			return $logger;
		},
		'db' => function (ContainerInterface $c) {
			$settings = $c->get('settings')['db'];
			$pdo = new PDO("mysql:host=" . $settings['host'] . ";port=" . $settings['port'] . ";dbname=" . $settings['dbname'] . ';charset=utf8',
				$settings['user'], $settings['pass'],[PDO::MYSQL_ATTR_LOCAL_INFILE => true]);
			$pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
			$pdo->setAttribute(PDO::ATTR_DEFAULT_FETCH_MODE, PDO::FETCH_ASSOC);
			$pdo->setAttribute(PDO::ATTR_EMULATE_PREPARES, false);
			$pdo->setAttribute(PDO::ATTR_STRINGIFY_FETCHES, false);
			return $pdo;
		},
		'rabbitmq' => function (ContainerInterface $c) {
			$settings = $c->get('settings')['rabbitmq'];
			if ($_ENV['ENV'] === 'aws_prod') {
				return null;
			} else {
				return new AMQPStreamConnection($settings['host'], $settings['port'], $settings['user'], $settings['pass'], '/', false, 'AMQPLAIN', null, 'en_US', 10);
			}
		},
		'sqs' => function (ContainerInterface $c) {
			$settings = $c->get('settings')['sqs'];
			if ($_ENV['ENV'] === 'aws_prod') {
				// Create a new SQS client
				return new SqsClient([
					'region' => $settings['region'],
					'version' => 'latest',
					'QueueUrl' => $settings['url']
				]);
			} else {
				// Set AWS credentials and region
				$credentials = new Aws\Credentials\Credentials('ASIATPI72ZOPX5AAXTPN', 'xklFMtMTfRjabLVvTnZShzvFDjQz9DUhVzykSOsq','FwoGZXIvYXdzEGwaDB4FyyPs9CMB+6ZTxiKGAUCFzpcAj0gJQ+M+eonvawqfpWsukZpzXmjsmpwiyiSrBmtuV12B5AI/oNOJdY0vyarxiLbQI2CAjr+FwT4lo4jXjMCLZqFoJx+hBr9qNRiQNyASVzZiy5nfZp6KMMR85illARIMwQyk9/vInzKcNmBnxA+pu1QY/oPDKaUXN0nX0WkQrRlfKM2A+KEGMihqhHlN5+SLk32SAH4pshfvxLPho6t16Cxpr/QllyqFUIl4Fi4Criug');

				// Create a new SQS client
				return new SqsClient([
					'credentials' => $credentials,
					'region' => $settings['region'],
					'version' => 'latest',
					'QueueUrl' => $settings['url']
				]);
			}
		}
	]);
};
