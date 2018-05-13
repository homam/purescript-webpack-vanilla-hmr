import * as App from "./App.purs";
import {initialState} from './App1.purs';
import * as React from 'react'
import * as ReactDOM from 'react-dom'
import createBrowserHistory from "history/createBrowserHistory";
import { Route, Switch, Router } from "react-router";

const history = createBrowserHistory();


declare const module : any
declare global {
	interface Window { __puxLastState: any; }
}


class CounterApp extends React.Component {
	render() {
		const { component, app } = App.toReact(window.__puxLastState || initialState)();
		const Counter = component;

		if (module.hot) {
			// don't lose state while HMR
			app.state.subscribe(function (st) {
				window.__puxLastState = st;
			});	
		
		}

		return (
			<div className="App">
				<Counter history={ history } />
			</div>
		);
	}
}

// vanilla hot module reloading
// @see https://webpack.js.org/guides/hot-module-replacement/
/*
if(module.hot) {
	const app = App.main(window.__puxLastState || initialState)();
	// don't lose state while HMR
	app.state.subscribe(function (st) {
		window.__puxLastState = st;
	});
	module.hot.accept();
} else {
	App.main(initialState)();
}
*/

const MainRoutedComponent = () => <Router history={history}>
		<Route path='/' component={CounterApp} />
	</Router>



ReactDOM.render(<MainRoutedComponent />, document.getElementById("counterApp"));


