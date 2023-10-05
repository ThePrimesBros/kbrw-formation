// function alertMe(){
//     alert('Hello World');
// }

// function Greeting() {
//     // Create a React element using React.createElement
//     return React.createElement('div', null, 'Hey I was created from React!');
// }

// let Greeting = () => {
//     return React.createElement('div', null, 'Hey I was created from React!');
// }

// // Render the React element inside the "root" div
// const rootElement = document.getElementById('root');
// ReactDOM.render(React.createElement(Greeting), rootElement);

let createReactClass = require('create-react-class')

let Page = createReactClass({
  render() {
    return (
      <JSXZ in="template" sel=".container">
        <Z sel=".item">Burgers</Z>
        <Z sel=".price">50</Z>
      </JSXZ>
    );
  }
});

ReactDOM.render(
    <Page />,
    document.getElementById('root')
)
