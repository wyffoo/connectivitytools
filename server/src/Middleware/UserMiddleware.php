<?php
declare(strict_types=1);

namespace App\Middleware;

use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Psr\Http\Server\MiddlewareInterface as Middleware;
use Psr\Http\Server\RequestHandlerInterface as RequestHandler;

class UserMiddleware implements Middleware
{
	/**
	 * {@inheritdoc}
	 */
	public function process(Request $request, RequestHandler $handler): Response
	{
		session_start();
		$request = $request->withAttribute('user',
			isset($_SESSION['authenticated_user']) ? $_SESSION['authenticated_user'] : null);
		return $handler->handle($request);
	}
}
