# log-classifier

Original version by Hiroto Shioi (https://github.com/HirotoShioi).

The purpose of this project is to analyze Cardano log file and provide a solution to the end user while collecting statistics so Cardano developers can prioritize the issues.

## Installation and Configuration Instructions
Please read the instructions available in [INSTALL.md](INSTALL.md)

## What it is

![log-classifier](https://user-images.githubusercontent.com/6264437/40305397-b5f182f8-5cfa-11e8-822e-c0c74d3dbce0.png)

- Log-classifier is a [command line interface](https://en.wikipedia.org/wiki/Command-line_interface) IOHK help desk agents can use to classify tickets.
- Currently, identifying the issue manually takes a decent amount of time and knowledge. Using classifier, the agent will be able to automate the identification of the issue without having to know all the known issues.
- This is a Haskell program so the agent will need [stack](https://docs.haskellstack.org/en/stable/README/) and [Nix](https://nixos.org/) in order to use the classifier. Instruction on how to use it will be documented on different file.
- The classifier cannot find any unknown/unique issues.

## How it works

![system architecture](https://user-images.githubusercontent.com/15665039/40042756-5d92611e-585d-11e8-80b4-72677c451dd1.png)<br/>
This is a use case diagram. Use case diagrams overview the usage requirements for a system. They are useful for presentations to management and/or project stakeholders, but for actual development, you will find that use cases provide significantly more value because they describe "the meat" of the actual requirements. For more details, please see [here](http://www.agilemodeling.com/artifacts/useCaseDiagram.htm)

### Overview

- Many of the Daedalus's issues can be identified by analyzing the log file. The classifier will utilize this by analyzing the log file and map with possible solution and problem which can be provided to the end user.
- Ticket sent from Daedalus bug report has log file attached. The classifier will analyze the log file that is attached to the ticket. If any known issues where found, it'll then post a comment to that ticket. (See example below)
- It uses [CSV](https://en.wikipedia.org/wiki/Comma-separated_values) (basically small record file) which contains all the information need to perform the analysis. This makes it easy to add new known issues.
- Help desk agent will use [command line interface](https://en.wikipedia.org/wiki/Command-line_interface) in order to run the classifier.
- The classifier will use the [Zendesk API](https://developer.zendesk.com/rest_api/docs/core/introduction) to perform any action. Because of this, agents will need to provide their email address and password to the classifier.

## Requirements

In order to use this, the user must possess an IOHK Zendesk agent ID and API key.

## Features

### Provide error text, problem, a solution to the ticket

The classifier will analyze the log file attached to the ticket. If any issues where found, it will post a comment which states

- Error code
- What is the issue
- Possible solution
- Error text classifier caught in the log file that is related to the issue.

See example below.
<br/>
<img src="https://user-images.githubusercontent.com/15665039/39680438-b148ef40-51db-11e8-9d51-f555cebde807.png" alt="analysis" style="width: 700px;"/>

### Add tags to the tickets

After classifier has done analysis on the ticket, it'll add tags to the ticket which can be used to identify what the issues are. Agents can later use this tags to collect statistics. <br />

<img src="https://user-images.githubusercontent.com/15665039/39680413-6c3970a0-51db-11e8-81d9-8c0faf53d1af.png" alt="tags"/><br/>

### Provide statistics of the Zendesk

The classifier can collect all the tags assigned to the ticket, accumulate them and provide to the Zendesk agent as statistics.

```terminal
There are currently **** tickets in the system assigned to *********@iohk.io
*** tickets have been analyzed by the classifier.
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

[Report server](https://github.com/input-output-hk/cardano-report-server) had a bug where it assigned both requester and assignees as report server and write down user's address in the comment section. This made it so that help desk agent where unable to reply back to the user unless agents create new tickets manually which takes some time to reply back. Note that this issue is already fixed.
The classifier can collect email addresses of tickets with this issue.

## Simple use-case scenario

### IOHK help desk agent finds ticket submitted from the end user with log file attached and wants to perform analysis on it

In this case, one can run the command below.

```terminal
./log-classifier-exe process-ticket <TICKET_ID>
```

This will parse the log file attached to the `<TICKET_ID>` and provide the result to the agent using private comment in the Zendesk UI. The agent then use that comment to help troubleshoot the end user.

### IOHK help desk agent wants to parse every ticket that is sent from Daedalus bug report with log file attached

In this case, one can run the command below.

```terminal
./log-classifier-exe process-tickets
```

This will parse any tickets with log file attached that are sent from the Daedalus bug report. It then post analysis result as private comment on any ticket that it has parsed so that the agents can provide the end user with solution.

### IOHK help desk agent wants to know the statistics of the occurring issues so he/she can report to the Cardano-SL team which will categorize/prioritize the issue

In this case, one can run the command below.

```terminal
./log-classifier-exe show-stats
```

This will collect all the tags that are attached to the ticket then group them so that the agent can report the dev team. The team will then categorize/prioritize the issue.

### Some of the tickets has a issue where requester and assignee is report server itself therefore agent must create a new ticket to contact with end user which takes a decent amount of time. Agent wants facilitate this by collecting email addresses of these tickets so he/she can send batch emails to these users

In this case, one can run the command below.

```terminal
./log-classifier-exe collect-emails
```

This will collect all the email addresses with the ticket where the assignee and the requester is report server and write them on a file `emailAddress.txt`. Agent then can later use the text file to send batch emails to the end user.

*Note that this issue has been fixed (meaning all the ticket has appropriate requester assigned to it therefore the agent does not need to create a new ticket to contact with the end user.) so this command is deprecated.
