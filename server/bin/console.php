<?php

use Psr\Container\ContainerInterface;
use Symfony\Component\Console\Input\ArgvInput;
use App\Console;

require_once __DIR__ . '/../vendor/autoload.php';

$input = new ArgvInput();
$env = $input->getParameterOption(['--env', '-e'], 'development');

if ($env) {
	$_ENV['APP_ENV'] = $env;
}

/** @var ContainerInterface $container */
$application = (require __DIR__ . '/../app/bootstrap.php');
$container = $application->getContainer();

$commands = $container->get('settings')['commands'];

$class = $commands[$input->getFirstArgument()];
$command = new $class($container, $class, $input);
$command->execute();
