# Usage

This document is written for a scenario where an agent wants to use Postman for interacting with the application.

This is the list of steps to be performed when trying to run the application using Postman:
1. Download Postman - go to [https://www.getpostman.com/](https://www.getpostman.com/) and go to "Download app". You can find the specific download location at [https://www.getpostman.com/apps](https://www.getpostman.com/apps). Select your platform and install it.

![image](https://user-images.githubusercontent.com/6264437/47214731-2ba08100-d39f-11e8-9a3e-06bbf7e2c486.png)

2. Please follow the installation instructions. Click on "Download" and when the download is complete, unzip the contents where you want to install the application and you should have something like the image below.

![image](https://user-images.githubusercontent.com/6264437/47214843-79b58480-d39f-11e8-9869-78bb02778902.png)

3. After that, run the application by clicking on the "Postman" file. You should get a windows simlar to the image below.

![image](https://user-images.githubusercontent.com/6264437/47215018-298af200-d3a0-11e8-93e1-c81be44ed351.png)

4. Next, import the requests you require.

 Go to the address [here](https://www.getpostman.com/collections/018eec85b244921f8b52) and simply copy the url in the address bar.

-Next with your Postman application running, click the import button at the top of the Postman Application
-Select the Import From Link Tab
-Paste the link you just copied into this text field
-Click the import button

The result should be seen on the left window called "Collections".

![image](https://user-images.githubusercontent.com/6264437/47215355-77ecc080-d3a1-11e8-9ea2-c44c0b61fde4.png)

5. Update your username and password for each of the requests in the collection you just added. 

There are 3 requests:
- Get ticket
- Get ticket comments
- Analyze ticket

*For each one*, you should do the following:
- select a request (I will select "Get ticket", but all others should be the same)
- go to "Authorization" and select "Basic auth"

![image](https://user-images.githubusercontent.com/6264437/47215606-54764580-d3a2-11e8-9f40-7a56afc0ef10.png)

- insert your username/password in the fields and after that, click on "Update request".

![image](https://user-images.githubusercontent.com/6264437/47215681-9dc69500-d3a2-11e8-8cf9-fe85d882cd54.png)

- after that, click "Save", which is next to "Send".

Repeat this for all other requests!

6. You are done! You can now use the application. For example, if you want to use the analysis, just click on "Analyze ticket" and after that go to "Body" and replace the ticket id with the one you want to analyze. After the analysis is complete, you will see a response in the Postman application, as well as the ticket itself.

