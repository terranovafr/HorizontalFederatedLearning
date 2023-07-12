# Horizontal Federated Learning
In this project, an Erlang Communication Layer and a Java Web Server were realized for the support of Horizontal Federated Learning Techniques, with a particular focus on KMeans as the target Machine Learning Algorithm.

## Distributed Software Architecture
The software architecture of the Erlang nodes is layered as described in the following diagram.<br>
<img width="873" alt="image" src="https://github.com/terranovaa/HorizontalFederatedLearning/assets/61695945/326dfcb9-837f-47da-abc8-607a09368b8c"><br>
The supervisor process and the server process will be hosted in the same node, while the client processes can be hosted in the same or different nodes.<br>
One pyrlang process will be available in each of the client nodes.<br>
Pyrlang is a Python library that implements the Erlang distribution protocol and creates an Erlang-compatible node in your Erlang cluster.
- The supervisor is in charge of supervision & control for a designated experiment.
- The server is in charge of monitoring the execution of rounds.
- The client process will be in charge of executing the algorithm rounds.
The project architecture considering both the Java Web Server and the distributed Erlang nodes is described in the following diagram.<br>
<img width="983" alt="image" src="https://github.com/terranovaa/HorizontalFederatedLearning/assets/61695945/774a78e5-761f-44a8-809e-e318f6f20016"><br>
The web application uses the three-tier architecture:
- Presentation Layer. This layer interacts with the client and it’s built using JSPs, HTML, CSS, and Javascript.
- Logic Layer. This layer contains the logic of the application. One part of this layer is written in Java and uses a Tomcat Server, while the other is written using Erlang.
- Data Access Layer. This layer consists of a Key/Value Repository and the corresponding DBMS.

## Project Structure
The project is organized as follows:
- ErlangCommunicationLayer/ contains the source code of the Erlang nodes (supervisor, server, client)
- JavaWebServer/ contains the source code of the Java Web Server
More information can be found in the documentation file attached.

## Execution Proof
We've proven that our algorithm is able to train in a distributed and privacy-oriented fashion the K-Means algorithm, reaching the same result that it could reach if it was centralized.<br>
<img width="687" alt="image" src="https://github.com/terranovaa/HorizontalFederatedLearning/assets/61695945/5d4358c0-8b00-44f7-b8f3-cfba14996f0a">
