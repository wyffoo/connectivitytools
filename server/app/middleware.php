<?php
declare(strict_types=1);

use App\Middleware\UserMiddleware;
use App\Middleware\JsonBodyParserMiddleware;
use Slim\App;

return function (App $app) {
	$app->add(UserMiddleware::class);
	$app->add(JsonBodyParserMiddleware::class);
};
