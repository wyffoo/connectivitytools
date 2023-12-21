<?php
declare(strict_types=1);

namespace App\Controllers;

use App\Domain\DomainException\DomainRecordNotFoundException;
use PDO;
use Psr\Container\ContainerInterface;
use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Psr\Log\LoggerInterface;
use Slim\Exception\HttpBadRequestException;
use Slim\Exception\HttpException;
use Slim\Exception\HttpNotFoundException;
use Slim\Exception\HttpUnauthorizedException;

abstract class Controller
{
    /**
     * @var LoggerInterface
     */
    protected $logger;

	/**
	 * @var PDO
	 */
	protected $db;

	/**
     * @var Request
     */
    protected $request;

    /**
     * @var Response
     */
    protected $response;

    /**
     * @var array
     */
    protected $args;

	/**
	 * @var ContainerInterface
	 */
    protected $container;

	/**
	 * @param ContainerInterface $container
	 * @param LoggerInterface $logger
	 */
    public function __construct(ContainerInterface $container, LoggerInterface $logger)
    {
    	$this->container = $container;
        $this->logger = $logger;
		$this->db = $container->get('db');
    }


    public function __call($name, $arguments): Response
    {

        $this->request = $arguments[0];
        $this->response = $arguments[1];
        $this->args = $arguments[2];
        try {
            return $this->{"action_$name"}();
        } catch (DomainRecordNotFoundException $e) {
            throw new HttpNotFoundException($this->request, $e->getMessage());
        } catch (\Exception $e) {
        	throw new HttpException($this->request, $e->getMessage(), 500);
		}
    }

    /**
     * @return array|object
     * @throws HttpBadRequestException
     */
    protected function getFormData()
    {
        $input = json_decode(file_get_contents('php://input'));

        if (json_last_error() !== JSON_ERROR_NONE) {
            throw new HttpBadRequestException($this->request, 'Malformed JSON input.');
        }

        return $input;
    }

    /**
     * @param  string $name
     * @return mixed
     * @throws HttpBadRequestException
     */
    protected function resolveArg(string $name)
    {
        if (!isset($this->args[$name])) {
            throw new HttpBadRequestException($this->request, "Could not resolve argument `{$name}`.");
        }

        return $this->args[$name];
    }

    /**
     * @param  array|object|null $data
     * @return Response
     */
    protected function respondWithData($data = null, int $statusCode = 200): Response
    {
        $payload = new ControllerPayload($statusCode, $data);

        return $this->respond($payload);
    }

    /**
     * @param ControllerPayload $payload
     * @return Response
     */
    protected function respond(ControllerPayload $payload): Response
    {
        $json = json_encode($payload, JSON_PRETTY_PRINT);
        $this->response->getBody()->write($json);

        return $this->response
                    ->withHeader('Content-Type', 'application/json')
                    ->withStatus($payload->getStatusCode());
    }

	/**
	 *
	 */
    protected function authorize() {
		if (!($current_user = $this->request->getAttribute('user'))) {
			throw new HttpUnauthorizedException($this->request, "Access denied.");
		}
		return $current_user;
	}

	protected function deny() {
		throw new HttpUnauthorizedException($this->request, "Access denied.");
	}
}

function remove_emoji($string) {

	// Match Emoticons
	$regex_emoticons = '/[\x{1F600}-\x{1F64F}]/u';
	$clear_string = preg_replace($regex_emoticons, '', $string);

	// Match Miscellaneous Symbols and Pictographs
	$regex_symbols = '/[\x{1F300}-\x{1F5FF}]/u';
	$clear_string = preg_replace($regex_symbols, '', $clear_string);

	// Match Transport And Map Symbols
	$regex_transport = '/[\x{1F680}-\x{1F6FF}]/u';
	$clear_string = preg_replace($regex_transport, '', $clear_string);

	// Match Miscellaneous Symbols
	$regex_misc = '/[\x{2600}-\x{26FF}]/u';
	$clear_string = preg_replace($regex_misc, '', $clear_string);

	// Match Dingbats
	$regex_dingbats = '/[\x{2700}-\x{27BF}]/u';
	$clear_string = preg_replace($regex_dingbats, '', $clear_string);

	return $clear_string;
}
