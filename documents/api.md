##Users

####`GET /users`
Retrieve users, sysadmin access

####``POST /users``
Create user, sysadmin access

####``POST /api/projects/sendInvite``
Invite user to project
```yaml
{
	"project_id": Number,
	"email": String
}
```
```

####``GET /users/me``
Retrieve single user, authenticated user 

####``GET /users/project/:id``
Retrieve users associated with project by :id, project.user_id = authenticated user 

####``GET /users/my_projects``
Retrieve users associated with authenticated user projects, admin access 

Response examples:
```json
[
  {
	"id": 1,
	"name": "Петя Иванов",
	"email": "pet@medoc.ua",
	"projects": [
		{
			"id": 1,
			"name": "Проект 1"
		}
	]
  }
]
```

####``DELETE /users/:id``
Delete user, sysadmin access

####``PUT /users/:id``
Update user, sysadmin access

####``PUT /users/me``
Update user, authenticated user

##Objects

####``GET /objects?limit=100&offset=0&name=object_name``
Retrieve objects associated with authenticated user

Response examples:
```json
{ 
	"total": 23912,
	"offset": 0,
	"records":
	[
	  {
		"id": 1,
		"name": "Объект 1",
		"project_name": "Проект 1"
	  },
	  {
		"id": 2,
		"name": "Объект 2",
		"project_name": "Проект 1"
	  }
	]
}
```

####``GET /objects/project/:id?limit=100&offset=0``
Retrieve objects, where project_id = :id, user_id = authenticated user

Response example:
```json
{ 
	"total": 23912,
	"offset": 0,
	"records":
	[   
	  {"id":1,"name":"Объект 1","isCompleted":true,"isCalculated":false},
	  {"id":2,"name":"Объект 2","isCompleted":false,"isCalculated":false},
	  {"id":3,"name":"Объект 3","isCompleted":true,"isCalculated":true},
	  {"id":4,"name":"Объект 4","isCompleted":true,"isCalculated":false},
	  {"id":5,"name":"Объект 5","isCompleted":false,"isCalculated":false},
	  {"id":6,"name":"Объект 6","isCompleted":true,"isCalculated":true},
	  {"id":7,"name":"Объект 7","isCompleted":true,"isCalculated":false},
	  {"id":8,"name":"Объект 8","isCompleted":false,"isCalculated":false},
	  {"id":9,"name":"Объект 9","isCompleted":true,"isCalculated":true},
	  {"id":10,"name":"Объект 10","isCompleted":true,"isCalculated":false},
	  {"id":11,"name":"Объект 11","isCompleted":false,"isCalculated":false},
	  {"id":12,"name":"Объект 12","isCompleted":true,"isCalculated":true}
	]
}
```
isCompleted - all object params are set 
isCalculated - object has calculation results

####``POST /objects``
Create object, user_id = authenticated user

####``PUT /objects/:id``
Update object, user_id = authenticated user

####``DELETE /objects/:id``
Delete objects, user_id = authenticated user

##Technologies

####``GET /technologies``
Retrieve technologies

Response example:
```json
[
  {
	"id": 1,
	"name": "WiFi+Ethernet_c+Ethernet_f",
	"description": "WiFi - на уровне доступа\nEthernet на медных кабелях - на уровне распределения\nEthernet оптика  - на восстановительном сегменте"
  },
  {
	"id": 2,
	"name": "WiFi+WiFi+WiFi",
	"description": "WiFi на всех уровнях"
  }
]
```

####``GET /technologies/project/:id``
Retrieve technologies associated with project_id by :id, user_id = authenticated user

Response example:
```json
[
  {
	"id": 1,
	"name": "WiFi+Ethernet_c+Ethernet_f",
	"isComplete": true
  },
  {
	"id": 2,
	"name": "WiFi+WiFi+WiFi",
	"isComplete": false
  },
  {
	"id": 3,
	"name": "WiFi+WiFi+Ethernet_f",
	"isComplete": true
  }
]
```
isComplete - all technology params are set 

####``POST /technologies``
Created technology, sysadmin access
```yaml
{
	"name": String,
	"description: String
}
```

####``PUT /technologies/:id``
Update technology, sysadmin access 
```yaml
{
	"name": String,
	"description: String
}
```


####``DELETE /technologies/:id``
Delete technology, sysadmin access 

##Projects

####``GET /projects``
Retrieve projects, where user_id = authenticated user

Response example:
```json
[
  {
	"id": 1,
	"identifier": "test_ident1",
	"name": "Тестовый проект 1",
	"description": "Описание тестового проекта"
  },
  {
	"id": 2,
	"identifier": "test_ident2",
	"name": "Тестовый проект 2",
	"description": "Описание тестового проекта"
  }
]
```

####``GET /projects/:id``
Retrieve project by id, user_id = authenticated user

Response example:
```json
{
  "id": 1,
  "name": "Тестовый проект",
  "description": "Описание тестового проекта",
  "objects": [
	{
	  "id": 1,
	  "name": "Объект 1",
	  "designer": {
		"id": 3,
		"name": "Проектировщик 3"
	  }
	},
	{
	  "id": 2,
	  "name": "Объект 2",
	  "designer": {
		"id": 2,
		"name": "Проектировщик 2"
	  }
	},
	{
	  "id": 3,
	  "name": "Объект 3",
	  "designer": {
		"id": 1,
		"name": "Проектировщик 1"
	  }
	}
  ],
  "technologies": [
	{
	  "id": 1,
	  "name": "WiFi+Ethernet_c+Ethernet_f",
	  "description": "WiFi - на уровне доступа\nEthernet на медных кабелях - на уровне распределения\nEthernet оптика - на восстановительном сегменте"
	}
  ],
  "isPublic": true
}
```

####``POST /projects``
Create project, user_id = authenticated user
```yaml
{
	"name": String,
	"description": String,
	"objects":Array.of({		
		"name": String,
		"designer":	{"id": Number}
	}),
	"technologies":Array.of({
		"id": Number
	}),
	"isPublic": Boolean
}
```

####``PUT /projects/:id``
Update project, user_id = authenticated user
```yaml
{
	"name": String,
	"description": String,
	"objects": Array.of({
		"name": String, "designer": {"id": Number}
	}),
	"technologies": Array.of({
		"id": Number
	}),
	"isPublic": Boolean
}
```

####``DELETE /projects/:id``
Delete project, user_id = authenticated user

####``POST /projects/request/confirm``
Confirm join project request from designer
```yaml
{
	"request_id": Number,
	"project_id": Number
}
```

####``POST /projects/request/decline``
Decline join project request from designer
```yaml
{
	"request_id": Number,
	"project_id": Number
}
```

####``POST /projects/removeDesigner``
Remove designer from the project
```yaml
{
	"user_id": Number,
	"project_id": Number
}
```

##Variables

####``GET /variables/class/:value``
Retrieve variables, where class = :value 

Response example:
```json
[
  {
	"id": 3,
	"group": "GeographicalParametersSet",
	"name": "GeographicalParametersSet.Square",
	"description": "Площадь проектирования",
	"type": "input",
	"default_value": "1",
	"unit": "кв.км",
	"required": 1
  },
  {
	"id": 2,
	"group": "GeographicalParametersSet",
	"name": "GeographicalParametersSet.LocationType",
	"description": "Тип местности",
	"type": "select",
	"default_value": "3",
	"unit": "",
	"required": 0,
	"options": [
	  {
		"value": "1",
		"name": "Равнинная местность"
	  },
	  {
		"value": "2",
		"name": "Горная"
	  },
	  {
		"value": "3",
		"name": "Смешанная"
	  }
	]
  }
]
```

####``GET /variables/project/:id``
Retrieve variables associated with project by :id, user_id = authenticated user 

Response example:
```json
[
  {
	"id": 3,
	"value": "1"
  },
  {
	"id": 2,
	"value": "3"
  }
]
```

####``POST /variables/project/:id``
Add or update project variables. Make sure that project owner = authenticated user. 
```yaml
{
	"variables": Array.of({
		"id": Number,
		"value": String
	})
}
```

####``GET /variables/object/:id``
Retrieve variables associated with object by :id, user_id = authenticated user 

Response example:
```json
[
  {
	"id": 3,
	"value": "1"
  },
  {
	"id": 2,
	"value": "3"
  }
]
```

####``POST /variables/object/:id``
Add or update object variables. Make sure that object owner = authenticated user. 
```yaml
{
	"variables": Array.of({
		"id": Number,
		"value": String
	})
}
```

####``GET /variables/technology/:id/project/:project_id``
Retrieve variables associated with technology by :id and project_id, user_id = authenticated user 

Response example:
```json
[
  {
	"id": 3,
	"value": "1"
  },
  {
	"id": 2,
	"value": "3"
  }
]
```

####``POST /variables/technology/:id``
Add or update object variables. Make sure that project+technology owner = authenticated user. 
```yaml
{
	"project_id": Number,
	"variables": Array.of({
		"id": Number,
		"value": String
	})
}
```

##Results

####``GET /results/project/:id``
Retrieve results associated with project by :id, project.user_id = authenticated user

Response example:
```json
[
  {"result_id":1, "object_name": "Объект 1", "technology_name": "Набор технологий 1", "nvp": 15000},
  {"result_id":2, "object_name": "Объект 1", "technology_name": "Набор технологий 2", "nvp": 1500},
  {"result_id":3, "object_name": "Объект 2", "technology_name": "Набор технологий 1", "nvp": 0}
]
```

####``GET /results/object/:id``
Retrieve results associated with object by :id, project.user_id = authenticated user

Response example:
```json
[
   	{"result_id":1, "technology_name": "WiFi+Ethernet_c+Ethernet_f", "nvp": 150000},
	{"result_id":2, "technology_name": "WiFi+WiFi+WiFi", "nvp": 1500},
	{"result_id":3, "technology_name": "WiFi+WiFi+Ethernet_f", "nvp": 0}
   
]
```

####``GET /results/object/:object_id/technology/:technology_id``
Retrieve results associated with object by :object_id and technology by :technology_id 
object.user_id = authenticated user

Response example:
```json
[
  {"result_id":1, "object_name": "Объект 1", "technology_name": "Набор технологий 1", "nvp": 15000},
  {"result_id":2, "object_name": "Объект 1", "technology_name": "Набор технологий 2", "nvp": 1500},
  {"result_id":3, "object_name": "Объект 2", "technology_name": "Набор технологий 1", "nvp": 0}
]
```

##Calculations

####``POST /calculation/project/:id``
Request project calculation by project :id, project.user_id = authenticated user 

Response: HTTP code 200 or Error

####``POST /calculation/object/:id``
Request object calculation by object :id, object.user_id = authenticated user 

Response: HTTP code 200 or Error

####``GET /calculation/project/:id``
Retrieve project calculation results by project :id, project.user_id = authenticated user

Response example:
```json
[
  {"result_id":1, "object_name": "Объект 1", "technology_name": "Набор технологий 1", "nvp": 15000},
  {"result_id":2, "object_name": "Объект 1", "technology_name": "Набор технологий 2", "nvp": 1500},
  {"result_id":3, "object_name": "Объект 2", "technology_name": "Набор технологий 1", "nvp": 0},
  {"result_id":4, "object_name": "Объект 2", "technology_name": "Набор технологий 2", "nvp": -4200},
  {"result_id":5, "object_name": "Объект 3", "technology_name": "Набор технологий 1", "nvp": 2719},
  {"result_id":6, "object_name": "Объект 3", "technology_name": "Набор технологий 2", "nvp": 42420},
  {"result_id":7, "object_name": "Объект 3", "technology_name": "Набор технологий 3", "nvp": -100500}
]
```

####``GET /calculation/report/project/:id``
Retrieve project calculation results in PDF by project :id, project.user_id = authenticated user

####``GET /calculation/object/:id``
Retrieve object calculation results by object :id, object.user_id = authenticated user

Response example:
```json
{
  "result_ids": [1, 2, 3],
  "nvps": [
	{"name": "Набор технологий 1", "nvp": 150000},
	{"name": "Набор технологий 2", "nvp": 1500},
	{"name": "Набор технологий 3", "nvp": 0}
  ]
}
```

####``GET /calculation/report/object/:id``
Retrieve object calculation results in PDF by object :id, object.user_id = authenticated user
