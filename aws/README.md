# AWS Toolkit for astro library

The toolkit builds, creates and updates AWS Lambda function for astro library.

It was inspired by:

* [aws-lambda-haskell](https://github.com/abailly/aws-lambda-haskell)
* System.Docker module from [system-extra](https://github.com/abailly/system-extra)
* [introToAWSLambda](https://github.com/chadbrewbaker/introToAWSLambda)


## How to use

### Prerequisites

* [AWS CLI](https://aws.amazon.com/cli/)
* [Docker](https://www.docker.com/)

### Build the application

```
% rake build
```

### Create an AWS Lambda function

```
% rake aws_lambda_create
```

You probably need to update `aws_lambda_role_name` variable in `Rakefile`.
It should conntain Role ARN with standard policy for AWS Lambda functions:

```
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "logs:CreateLogGroup",
        "logs:CreateLogStream",
        "logs:PutLogEvents"
      ],
      "Resource": "arn:aws:logs:*:*:*"
    }
  ]
}
```
 
You need to create the function only once.

    
### Build and deploy new version of the Lambda function

```
% rake build
% rake aws_lambda_update
```


### Test the function

```
% rake aws_lambda_invoke
```

### Test query

```
{
    "coordinates": {
        "latitude":51, "longitude":0
    },
    "datetime":"2017-05-10T12:12:12.111111+01:00"
}
```
