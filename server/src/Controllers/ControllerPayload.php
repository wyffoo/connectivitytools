<?php
declare(strict_types=1);

namespace App\Controllers;

use JsonSerializable;

class ControllerPayload implements JsonSerializable
{
    /**
     * @var int
     */
    private $statusCode;

    /**
     * @var array|object|null
     */
    private $data;

    /**
     * @var ControllerError|null
     */
    private $error;

    /**
     * @param int                   $statusCode
     * @param array|object|null     $data
     * @param ControllerError|null      $error
     */
    public function __construct(
        int $statusCode = 200,
        $data = null,
        ?ControllerError $error = null
    ) {
        $this->statusCode = $statusCode;
        $this->data = $data;
        $this->error = $error;
    }

    /**
     * @return int
     */
    public function getStatusCode(): int
    {
        return $this->statusCode;
    }

    /**
     * @return array|null|object
     */
    public function getData()
    {
        return $this->data;
    }

    /**
     * @return ControllerError|null
     */
    public function getError(): ?ControllerError
    {
        return $this->error;
    }

    /**
     * @return array
     */
    public function jsonSerialize()
    {
//        $payload = [
//            'statusCode' => $this->statusCode,
//        ];
//
//        if ($this->data !== null) {
//            $payload['data'] = $this->data;
//        } elseif ($this->error !== null) {
//            $payload['error'] = $this->error;
//        }

        return $this->data;
    }
}
