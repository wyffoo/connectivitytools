import json
import boto3

def lambda_handler(event, context):
	# TODO implement
	message = event["Records"].pop()
	message_body =  message["body"]
	event_source_arn = message['eventSourceARN']
	queue_name = event_source_arn.split(':')[-1]
	print(queue_name)

	ecs = boto3.client('ecs')

	# Specify your task definition and container name/ARN here
	task_definition = 'giga-calc'
	container_name = 'calc'
	if (queue_name == 'giga-calc-requests.fifo'):
		container_arn = '238974323615.dkr.ecr.eu-central-1.amazonaws.com/connectivitytools:giga-new-calc-worker'
	else:
		container_arn = '238974323615.dkr.ecr.eu-central-1.amazonaws.com/connectivitytools:giga-calc-worker'

	# Specify your ECS cluster and subnets/VPCs here
	ecs_cluster = 'giga'
	subnet_id_1 = 'subnet-023f824fb0daa9b67'
	subnet_id_2 = 'subnet-0d4f7f1ae1c594f57'
	security_group = 'sg-0680edf773046c5d6'

	# Define the parameters for your task
	task_params = {
		'taskDefinition': task_definition,
		'cluster': ecs_cluster,
		'count': 1,
		'launchType': 'FARGATE',  # or 'EC2'
		'networkConfiguration': {
			'awsvpcConfiguration': {
				'subnets': [subnet_id_1, subnet_id_2],
				'securityGroups': [security_group],
				'assignPublicIp': 'DISABLED'
			}
		},
		'overrides': {
			'containerOverrides': [
				{
					'name': container_name,
					'environment': [
						{
							'name': 'CALC_MESSAGE_BODY',
							'value': str(message_body)
						}
					]
				}
			]
		}
	}

	# Run the ECS task and get the task ARN
	response = ecs.run_task(**task_params)
	task_arn = response['tasks'][0]['taskArn']

	# response = ecs.describe_tasks(
	#     cluster='giga',
	#     tasks=[task_arn]
	# )

	print(response)
	# if response['failures']:
	#     print(response['failures'])
	# else:
	#     print(response['tasks'][0]['containers'][0]['exitCode'])
