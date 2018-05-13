import * as React from 'react'
import './Layout.less';

export const buttonClass = (props) => <button style={{ color: 'green'}} {...props}>{props.children}</button>