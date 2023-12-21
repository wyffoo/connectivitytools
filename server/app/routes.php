<?php
declare(strict_types=1);

use App\Controllers\LocationsController;
use App\Controllers\ProjectController;
use App\Controllers\ResultsController;
use App\Controllers\TrafficProfilesController;
use App\Controllers\UserController;
use App\Controllers\CalculationController;
use App\Controllers\VariablesController;
use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Slim\App;
use Slim\Interfaces\RouteCollectorProxyInterface as Group;

return function (App $app) {
	$app->options('/{routes:.*}', function (Request $request, Response $response) {
		// CORS Pre-Flight OPTIONS Request Handler
		return $response;
	});

	$app->post('/api/auth/local', UserController::class . ':signInAPI');

	$app->group('/api/users', function (Group $group) {
		$group->get('', UserController::class . ':getAllUsersAPI');
		$group->get('/me', UserController::class . ':authenticatedAPI');
		$group->post('/setLanguage', UserController::class . ':setLanguage');
		$group->post('/verifyEmailIsRegistered', UserController::class . ':verifyEmailIsRegisteredAPI');
		$group->post('/signout', UserController::class . ':signOutAPI');
		$group->post('/signup', UserController::class . ':signUpAPI');
		$group->post('/passwordreset', UserController::class . ':passwordResetAPI');
		$group->put('/password', UserController::class . ':passwordUpdateAPI');
		$group->post('/countries', UserController::class . ':updateUserCountriesAPI');
		$group->delete('/{id}', UserController::class . ':deleteUserAPI');
		$group->post('/status', UserController::class . ':updateUserStatusAPI');
	});

	$app->group('/api/locations', function (Group $group) {
		$group->get('', LocationsController::class . ':getLocationsAPI');
		$group->get('/countries', LocationsController::class . ':getAvailableCountriesAPI');
		$group->post('/dataset/{country_id}/{type}', LocationsController::class . ':importDatasetAPI');
		$group->get('/dataset/{country_id}/{type}', LocationsController::class . ':exportDatasetAPI');
//		$group->post('/setLanguage', LocationsController::class . ':setLanguage');
	});

	$app->group('/api/projects', function (Group $group) {
		$group->get('', ProjectController::class . ':getProjectsListAPI');
		$group->get('/list', ProjectController::class . ':getProjectsListAPI');
		$group->get('/input_templates/{flags}', ProjectController::class . ':getInputTemplatesAPI');
		$group->get('/input_templates_by_project/{id}', ProjectController::class . ':getInputTemplatesByProjectIdAPI');
		$group->get('/templatedata/{id}', ProjectController::class . ':downloadTemplateDataByProjectIdAPI');
		$group->get('/locationsdata/{id}', ProjectController::class . ':downloadInputLocationsDataByProjectIdAPI');
		$group->get('/inputtemplate/{id}/{project_name}', ProjectController::class . ':downloadInputTemplateByIdAPI');
		$group->get('/inputlocations/{id}/{project_name}/{type}', ProjectController::class . ':downloadInputLocationsByProjectIdAPI');
		$group->get('/verified_countries', ProjectController::class . ':getVerifiedCountriesAPI');
		$group->get('/countries', ProjectController::class . ':getCountriesListAPI');
		$group->get('/my_countries', ProjectController::class . ':getUserCountriesListAPI');
		$group->post('/{id}/files', ProjectController::class . ':addFilesToProjectAPI');
		$group->post('', ProjectController::class . ':createProjectAPI');
		$group->post('/duplicate/{id}', ProjectController::class . ':duplicateProjectAPI');
		$group->get('/{id}', ProjectController::class . ':getProjectByIdAPI');
		$group->get('/{id}/progress', ProjectController::class . ':getProjectProgressByIdAPI');
		$group->put('/{id}', ProjectController::class . ':updateProjectAPI');
		$group->delete('/{id}', ProjectController::class . ':deleteProjectAPI');
	});

	$app->group('/api/results', function (Group $group) {
		$group->get('/project/{id}', ResultsController::class . ':getResultsByProjectIdAPI');
		$group->get('/project/{id}/xlsx', ResultsController::class . ':downloadReportByProjectIdAPI');
		$group->get('/project/{id}/smart_xlsx', ResultsController::class . ':downloadSmartReportByProjectIdAPI');
		$group->get('/project/{id}/topology_graph', ResultsController::class . ':topologyGraphData');
		$group->get('/project/{id}/topology_summary', ResultsController::class . ':projectTopologySummary');
		$group->get('/project/{id}/summary', ResultsController::class . ':projectSummary');
		$group->get('/project/{id}/log', ResultsController::class . ':downloadCalculationLog');
		$group->get('/project/{id}/geojson', ResultsController::class . ':downloadCalculationGeoJSON');
	});

	$app->group('/api/calculation', function (Group $group) {
		$group->post('/bandwidth', CalculationController::class . ':calculateBandwidthAPI');
		$group->post('/project/{id}', CalculationController::class . ':calculateProjectAPI');
		$group->post('/stop/project/{id}', CalculationController::class . ':stopProjectAPI');
		$group->post('/project/topology/{id}', CalculationController::class . ':calculateProjectTopologyAPI');
	});

	$app->group('/api/tp', function (Group $group) {
		$group->post('/service', TrafficProfilesController::class . ':createServiceAPI');
		$group->put('/service', TrafficProfilesController::class . ':updateServiceAPI');
		$group->delete('/service/{id}', TrafficProfilesController::class . ':deleteServiceAPI');
		$group->get('/profile/{id}', TrafficProfilesController::class . ':getProfileAPI');
		$group->post('/profile', TrafficProfilesController::class . ':createProfileAPI');
		$group->delete('/profile/{id}', TrafficProfilesController::class . ':deleteProfileAPI');
		$group->put('/profile/{id}', TrafficProfilesController::class . ':updateProfileAPI');
		$group->get('/default', TrafficProfilesController::class . ':getDefaultListAPI');
		$group->get('/custom', TrafficProfilesController::class . ':getCustomListAPI');
		$group->get('/traffic_sources', TrafficProfilesController::class . ':getTrafficSourcesListAPI');
		$group->get('/services', TrafficProfilesController::class . ':getServicesListAPI');
		$group->get('/records/{id}', TrafficProfilesController::class . ':getProfileRecordsAPI');
	});

	$app->group('/api/variables', function (Group $group) {
		$group->get('/project/{id}', VariablesController::class . ':getVariablesByProjectIdAPI');
		$group->post('/project/{id}/xlsx', VariablesController::class . ':addVariablesFromXLSXToProjectAPI');
		$group->get('/project/{id}/xlsx', VariablesController::class . ':downloadVariablesByProjectIdAPI');
		$group->delete('/reset/project/{id}', VariablesController::class . ':restoreDefaultVariablesAPI');
	});
};
