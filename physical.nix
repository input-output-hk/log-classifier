let

  region = "eu-west-1";
  accessKeyId = "logclass"; # symbolic name looked up in ~/.ec2-keys or a ~/.aws/credentials profile name

  ec2 =
    { resources, ... }:
    { deployment.targetEnv = "ec2";
      deployment.ec2.accessKeyId = accessKeyId;
      deployment.ec2.region = region;
      deployment.ec2.instanceType = "m3.medium";
      deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
    };

in
{ machine  = ec2;

  # Provision an EC2 key pair.
  resources.ec2KeyPairs.my-key-pair =
    { inherit region accessKeyId; };
}
