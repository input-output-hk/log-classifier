# log-classifier

The purpose of this project is to analyze tickets that are in the IOHK Zendesk system and provide solution to the end user while collecting statistics so Cardano developers can prioritize the issues.

## What it is

- Log-classifier is a [command line interface](https://en.wikipedia.org/wiki/Command-line_interface) IOHK Zendesk agents can use to classify tickets.
- Currently, identifying the issue manually takes decent amount of time and knowledge. Using classifier, agent will be able to automate the identification of the issue without having to know all the known issues.
- This is a Haskell program so agent will need [stack](https://docs.haskellstack.org/en/stable/README/) and [Nix](https://nixos.org/) in order to use the classifier. Instruction on how to use it will be documented on different file.
- Classifier cannot find any unknown/unique issues.

## How it works

![system architecture](https://user-images.githubusercontent.com/15665039/39682475-1ef59a7a-51eb-11e8-8ca3-0632feae1638.png)

- Many of the Daedalus's issues can be identified by analyzing the log file. Classifier will utilize this by analyzing the log file and map with possible solution and problem which can be provided to the end user.
- Ticket sent from Daedalus bug report has log file attached. Classifier will analyze the log file that is attached to the ticket. If any known issues where found, it'll then post an comment to that ticket. (See example below)
- It uses [CSV](https://en.wikipedia.org/wiki/Comma-separated_values) (basically small record file) which contains all the information need to perform analysis. This make it so it is easy to add new known issues.
- Zendesk agent will use [command line interface](https://en.wikipedia.org/wiki/Command-line_interface) in order to run the classifier.
- Classifier will use the [Zendesk API](https://developer.zendesk.com/rest_api/docs/core/introduction) to perform any action. Because of this, agents will need to provide Zendesk token to the classifier.

## Requirements

In order to use this, user must be IOHK Zendesk agent and be able to acquire Zendesk token.

## Features

### Provide error text, problem, solution to the ticket

Classifier will analyze the log file attached to the ticket. If any issues where found, it will post a comment which states

- Errror code
- What is the issue
- Possible solution
- Error text classifier caught in the log file that is related to the issue.

See example below.
<br/>
<img src="https://user-images.githubusercontent.com/15665039/39680438-b148ef40-51db-11e8-9d51-f555cebde807.png" alt="analysis" style="width: 700px;"/>

### Add tags to the tickets

After classifier has done analysis on the ticket, it'll add tags to the ticket which can be used to identify what the issues are. Agents can later use this tags to collect statistics of the Zendesk. <br />

<img src="https://user-images.githubusercontent.com/15665039/39680413-6c3970a0-51db-11e8-81d9-8c0faf53d1af.png" alt="tags"/><br/>

### Provide statistics of the Zendesk

Classifier can collect all the tags assigned to the ticket, accumulate them and provide to the Zendesk agent as statistics.

```terminal
There are currently **** tickets in the system assigned to *********@iohk.io
*** tickets has been analyzed by the classifier.
**** tickets are not analyzed.
Below are statistics:
analyzed: **
can-not-sync: **
cannot-get-db-size: **
cannot_connect_to_wallet_using_network: **
closed_by_merge: **
coin_redemption: **
connection-refused: **
db-corrupted: **
db-path-error: **
directory-not-found: **
.... and so on
```

### Collect email addresses of the tickets in which both requester and assignee is report server

[Report server](https://github.com/input-output-hk/cardano-report-server) had a bug where it assigned both requester and assignees as report server and write down user's address on the comment section. This made it so that Zendesk agent where unable to reply back to the user unless agents create new tickets manually which takes some time to reply back. Note that this issue is already fixed.
Classifier can collect email addresses of tickets with this issue.