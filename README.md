# Lambda Endpoint

## Requirements
- A postgresql server
- `libpq` installed

## Config
`/config/settings.yml` contains the port and database settings, please refer to `yesod`'s document.

`/config/lambda-settings.yml` contains the configuration specified for lambda project.

- `root-password`: since trace must be run in superuser mode, you should provide a root password here, this will be used in `sudo` command.

- `bpf/stap-path`: path to the executables.

- `submit-chunk-size`: http trace results submitting chuck size (in bytes).

- `platform-url`: url of the platform.

- `secret`: secret key to verify the identity.

- `endpoint-uuid`: uuid to identify endpoint.

## Authorization
The communication between the endpoint and the server must be verified. The method is to add a `Authorization` header. The format is as the following:
```
[uuid][argon2 of password]
```
example:
```
4633d686-f75d-4d1f-8415-02ce37d9e5f8$argon2i$v=19$m=4096,t=3,p=1$lxQKK/JGRLqc01yCPpJvSw$FUjxqpeiwORBGcgzPSytgHf1LTdbMLbGOyCqg61BJoM
```

## API
- `/heartbeat GET`: return a json with field `status` and `time`. `status` should be `alive` and `time` should be the utc time when replying the request.
- `/traces GET`: get all traces in the database. The format is as following
	```
	{
		"traceNo": 1, // database id
		"info": {
			"process": "/bin/bash", // the process to trace
			"funtions": ["xmalloc", "xfree"], // the functions to trace
			"environment": [], // additional environment variables
			"value": [], // correspoinding value to environment variables
			"options": [], // addtional options
			"traceType": [] // BPF or STAP
		}
	}
	```
- `/trace PUT`: put a trace template to the database. The content type must be set as `application/json`. The body format is as the following:
	```
	{
		"process": "/bin/bash", // the process to trace
		"funtions": ["xmalloc", "xfree"], // the functions to trace
		"environment": [], // additional environment variables
		"value": [], // correspoinding value to environment variables
		"options": [], // addtional options
		"traceType": [] // BPF or STAP
	}
	```
- `/trace/start POST`: start the trace. The content type must be set as `application/json`. The body format is as the following:
	```
	{
		"traceT": "STAP", // trace type
		"traceId": 1, // trace id (database)
		"lasting": 3600, // the lasting time of trace (in seconds) 
	}
	```
	The return form is also json:
	```
	{
		"thread": 54, // light weight haskell thread id
		"filePath": "/tmp/4633d686-f75d-4d1f-8415-02ce37d9e5f8.trace" // used as the identifier of a trace
	}
	```
	The trace info will be sent to the server via several HTTP PUT request. The url is "<platform-url>/submit"
	- At the beginning, a start request will be sent:
	  ```
	  {
		  "trace": "/tmp/4633d686-f75d-4d1f-8415-02ce37d9e5f8.trace",
		  "status": "start"
	  }
	  ```
	- Then, whenever the buffer limit is reached, a request contains chunked trace result will be sent.
	  ```
	  {
	  	  "trace": "/tmp/4633d686-f75d-4d1f-8415-02ce37d9e5f8.trace",
		  "no": <integer represents the chunk number (increasing)>,
		  "field": <BASE64 of current chunk info>
	  }
	  ```
	- When finished, a finishing request will be sent:
	  ```
	  {
		  "trace": "/tmp/4633d686-f75d-4d1f-8415-02ce37d9e5f8.trace",
		  "stderr": <as the name>,
		  "code": <exit code>,
		  "status": "finished"
	  }
	  ```
- `/trace/remove POST`: remove a trace from the database. The content type must be set as `application/json`. The body format is as the following:
	```
	{
		"removeType": "STAP", // trace type
		"removeId": 5 // trace id
	}
	```
- `/trace/running GET`: get a running trace info. Example url: `/trace/running?path=/tmp/4633d686-f75d-4d1f-8415-02ce37d9e5f8.trace`. The return format is json:
	```
	{
		"process": "/bin/bash", // the process to trace
		"funtions": ["xmalloc", "xfree"], // the functions to trace
		"environment": [], // additional environment variables
		"value": [], // correspoinding value to environment variables
		"options": [], // addtional options
		"traceType": [] // BPF or STAP
	}
	```
- `/traces/running/all GET`: get all running traces. The return type is json list. Objects is same as `/trace/running GET`.

- `/trace/kill POST`: kill a running trace. Return a plaintext `Killed`. It will not check whether the given process exists or not, but simply ignores those non-exsiting process. The request format should be
	```
	{
		"killPath": "/tmp/4633d686-f75d-4d1f-8415-02ce37d9e5f8.trace"
	}
	```
