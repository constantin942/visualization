(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[126],{93488:function(me,ee,e){"use strict";e.d(ee,{Z:function(){return Q}});var Z=e(28991),L=e(67294),E={icon:{tag:"svg",attrs:{viewBox:"64 64 896 896",focusable:"false"},children:[{tag:"path",attrs:{d:"M869 487.8L491.2 159.9c-2.9-2.5-6.6-3.9-10.5-3.9h-88.5c-7.4 0-10.8 9.2-5.2 14l350.2 304H152c-4.4 0-8 3.6-8 8v60c0 4.4 3.6 8 8 8h585.1L386.9 854c-5.6 4.9-2.2 14 5.2 14h91.5c1.9 0 3.8-.7 5.2-2L869 536.2a32.07 32.07 0 000-48.4z"}}]},name:"arrow-right",theme:"outlined"},O=E,U=e(27029),re=function(v,ce){return L.createElement(U.Z,(0,Z.Z)((0,Z.Z)({},v),{},{ref:ce,icon:O}))};re.displayName="ArrowRightOutlined";var Q=L.forwardRef(re)},37174:function(me){me.exports={alarm:"alarm___12xQi",salesRank:"salesRank___-5oIk",rankingList:"rankingList___rxbMU",rankingItemNumber:"rankingItemNumber___3eh0u",active:"active___3KjTZ",rankingItemTitle:"rankingItemTitle___3gCS2","alarm-data":"alarm-data___17B2L",alarmDetail:"alarmDetail___9Lz5b"}},52953:function(){},44887:function(){},26723:function(me,ee,e){"use strict";e.r(ee),e.d(ee,{default:function(){return et}});var Z=e(20228),L=e(11382),E=e(66456),O=e(78587),U=e(3182),re=e(57663),Q=e(71577),N=e(11849),v=e(2824),ce=e(94043),se=e.n(ce),t=e(67294),ue=e(37174),b=e.n(ue),I=e(48606),oe=e(71194),R=e(50146),K=e(88983),xe=e(47933),ze=e(62350),pe=e(75443),o=e(77576),ve=e(12028),fe=e(22385),g=e(61580),te=e(17462),Pe=e(76772),Ae=e(38663),we=e(52953),$=e(96156),Me=e(28481),Se=e(90484),Te=e(94184),de=e.n(Te),Ne=e(50344),he=e(24308),Ze=e(21687),_e=e(65632),y=e(22122);function ye(a){return a!=null}var Be=function(n){var r=n.itemPrefixCls,i=n.component,l=n.span,d=n.className,c=n.style,D=n.labelStyle,A=n.contentStyle,T=n.bordered,C=n.label,x=n.content,Y=n.colon,p=i;if(T){var f;return t.createElement(p,{className:de()((f={},(0,$.Z)(f,"".concat(r,"-item-label"),ye(C)),(0,$.Z)(f,"".concat(r,"-item-content"),ye(x)),f),d),style:c,colSpan:l},ye(C)&&t.createElement("span",{style:D},C),ye(x)&&t.createElement("span",{style:A},x))}return t.createElement(p,{className:de()("".concat(r,"-item"),d),style:c,colSpan:l},t.createElement("div",{className:"".concat(r,"-item-container")},(C||C===0)&&t.createElement("span",{className:de()("".concat(r,"-item-label"),(0,$.Z)({},"".concat(r,"-item-no-colon"),!Y)),style:D},C),(x||x===0)&&t.createElement("span",{className:de()("".concat(r,"-item-content")),style:A},x)))},Ce=Be;function Ee(a,n,r){var i=n.colon,l=n.prefixCls,d=n.bordered,c=r.component,D=r.type,A=r.showLabel,T=r.showContent,C=r.labelStyle,x=r.contentStyle;return a.map(function(Y,p){var f=Y.props,w=f.label,J=f.children,W=f.prefixCls,ne=W===void 0?l:W,G=f.className,m=f.style,ie=f.labelStyle,X=f.contentStyle,ae=f.span,H=ae===void 0?1:ae,M=Y.key;return typeof c=="string"?t.createElement(Ce,{key:"".concat(D,"-").concat(M||p),className:G,style:m,labelStyle:(0,y.Z)((0,y.Z)({},C),ie),contentStyle:(0,y.Z)((0,y.Z)({},x),X),span:H,colon:i,component:c,itemPrefixCls:ne,bordered:d,label:A?w:null,content:T?J:null}):[t.createElement(Ce,{key:"label-".concat(M||p),className:G,style:(0,y.Z)((0,y.Z)((0,y.Z)({},C),m),ie),span:1,colon:i,component:c[0],itemPrefixCls:ne,bordered:d,label:w}),t.createElement(Ce,{key:"content-".concat(M||p),className:G,style:(0,y.Z)((0,y.Z)((0,y.Z)({},x),m),X),span:H*2-1,component:c[1],itemPrefixCls:ne,bordered:d,content:J})]})}var je=function(n){var r=t.useContext(h),i=n.prefixCls,l=n.vertical,d=n.row,c=n.index,D=n.bordered;return l?t.createElement(t.Fragment,null,t.createElement("tr",{key:"label-".concat(c),className:"".concat(i,"-row")},Ee(d,n,(0,y.Z)({component:"th",type:"label",showLabel:!0},r))),t.createElement("tr",{key:"content-".concat(c),className:"".concat(i,"-row")},Ee(d,n,(0,y.Z)({component:"td",type:"content",showContent:!0},r)))):t.createElement("tr",{key:c,className:"".concat(i,"-row")},Ee(d,n,(0,y.Z)({component:D?["th","td"]:"td",type:"item",showLabel:!0,showContent:!0},r)))},Le=je,Ue=function(n){var r=n.children;return r},z=Ue,u=e(96159),h=t.createContext({}),F={xxl:3,xl:3,lg:3,md:3,sm:2,xs:1};function Ke(a,n){if(typeof a=="number")return a;if((0,Se.Z)(a)==="object")for(var r=0;r<he.c4.length;r++){var i=he.c4[r];if(n[i]&&a[i]!==void 0)return a[i]||F[i]}return 3}function Oe(a,n,r){var i=a;return(n===void 0||n>r)&&(i=(0,u.Tm)(a,{span:r}),(0,Ze.Z)(n===void 0,"Descriptions","Sum of column `span` in a line not match `column` of Descriptions.")),i}function Fe(a,n){var r=(0,Ne.Z)(a).filter(function(c){return c}),i=[],l=[],d=n;return r.forEach(function(c,D){var A,T=(A=c.props)===null||A===void 0?void 0:A.span,C=T||1;if(D===r.length-1){l.push(Oe(c,T,d)),i.push(l);return}C<d?(d-=C,l.push(c)):(l.push(Oe(c,C,d)),i.push(l),d=n,l=[])}),i}function be(a){var n,r=a.prefixCls,i=a.title,l=a.extra,d=a.column,c=d===void 0?F:d,D=a.colon,A=D===void 0?!0:D,T=a.bordered,C=a.layout,x=a.children,Y=a.className,p=a.style,f=a.size,w=a.labelStyle,J=a.contentStyle,W=t.useContext(_e.E_),ne=W.getPrefixCls,G=W.direction,m=ne("descriptions",r),ie=t.useState({}),X=(0,Me.Z)(ie,2),ae=X[0],H=X[1],M=Ke(c,ae);t.useEffect(function(){var _=he.ZP.subscribe(function(le){(0,Se.Z)(c)==="object"&&H(le)});return function(){he.ZP.unsubscribe(_)}},[]);var V=Fe(x,M),P=t.useMemo(function(){return{labelStyle:w,contentStyle:J}},[w,J]);return t.createElement(h.Provider,{value:P},t.createElement("div",{className:de()(m,(n={},(0,$.Z)(n,"".concat(m,"-").concat(f),f&&f!=="default"),(0,$.Z)(n,"".concat(m,"-bordered"),!!T),(0,$.Z)(n,"".concat(m,"-rtl"),G==="rtl"),n),Y),style:p},(i||l)&&t.createElement("div",{className:"".concat(m,"-header")},i&&t.createElement("div",{className:"".concat(m,"-title")},i),l&&t.createElement("div",{className:"".concat(m,"-extra")},l)),t.createElement("div",{className:"".concat(m,"-view")},t.createElement("table",null,t.createElement("tbody",null,V.map(function(_,le){return t.createElement(Le,{key:le,index:le,colon:A,prefixCls:m,vertical:C==="vertical",bordered:T,row:_})}))))))}be.Item=z;var ge=be,Ge=e(74379),De=e(38648),We=e(13312),Ye=e(1870),s=e(85893);function Je(a,n){var r=(0,t.useState)({}),i=(0,v.Z)(r,2),l=i[0],d=i[1],c=(0,t.useState)(!1),D=(0,v.Z)(c,2),A=D[0],T=D[1],C=(0,t.useState)(!1),x=(0,v.Z)(C,2),Y=x[0],p=x[1],f=(0,t.useState)(!1),w=(0,v.Z)(f,2),J=w[0],W=w[1],ne=(0,t.useState)([]),G=(0,v.Z)(ne,2),m=G[0],ie=G[1],X=(0,t.useState)("update"),ae=(0,v.Z)(X,2),H=ae[0],M=ae[1];return(0,t.useImperativeHandle)(n,function(){return{setInitValues:function(P){P.length===1&&d(P[0]),ie(P)}}}),(0,s.jsx)(R.Z,{width:600,afterClose:function(){p(!1),T(!1),M("update")},destroyOnClose:!0,title:m.length===1?"".concat(l==null?void 0:l.userName,"\uFF08\u544A\u8B66\u5904\u7F6E\uFF09"):"\u6279\u91CF\u5904\u7F6E".concat(m.length,"\u6761\u544A\u8B66"),visible:a.handleVisible,onCancel:function(){a.setVisible(!1),W(!1),M("update")},onOk:function(){if(A&&(0,I.QD)([{id:l.id,matchRuleId:l.matchRuleId,originalTime:l.originalTime,userName:l.userName,alarmContent:l.alarmContent,globalTraceId:l.globalTraceId,flag:J?"update":"delete",updateUserPortrait:J?1:0}]).then(function(_){var le=_.code;le===We.Gh&&(De.Z.info({message:"\u63D0\u793A",description:"\u64CD\u4F5C\u6210\u529F!"}),a.setVisible(!1),a.refresh())}),m.length>1){var P=m.map(function(_){return _.flag=H,_.updateUserPortrait=H==="delete"?1:0,_});(0,I.QD)(P).then(function(){a.refresh(),De.Z.info({message:"\u63D0\u793A",description:"\u64CD\u4F5C\u6210\u529F!"}),a.setVisible(!1)})}a.setVisible(!1)},children:m.length===1?(0,s.jsxs)(s.Fragment,{children:[(0,s.jsxs)(ge,{layout:"horizontal",column:2,children:[(0,s.jsx)(ge.Item,{label:"\u8BB0\u5F55\u4EA7\u751F\u65F6\u95F4",children:l==null?void 0:l.originalTime}),(0,s.jsx)(ge.Item,{label:"\u89C4\u5219ID",children:l==null?void 0:l.matchRuleId})]}),(0,s.jsx)(Pe.Z,{message:l==null?void 0:l.alarmContent,className:"text-c",type:"warning"}),(0,s.jsxs)("div",{className:"flex-x-sb mt-20",children:[(0,s.jsx)("div",{}),(0,s.jsx)("div",{children:(0,s.jsx)(pe.Z,{title:function(){return(0,s.jsxs)(g.Z,{title:"\u3010\u4E0D\u66F4\u65B0\u3011\u4EC5\u4EC5\u5FFD\u7565\u6B64\u6761\u544A\u8B66\u4FE1\u606F",children:["\u662F\u5426\u66F4\u65B0\u753B\u50CF\uFF1F",(0,s.jsx)(Ye.Z,{})]})},visible:Y,onConfirm:function(){p(!1),W(!0)},onCancel:function(){return p(!1)},okText:"\u66F4\u65B0",cancelText:"\u4E0D\u66F4\u65B0",children:(0,s.jsx)(ve.Z,{checkedChildren:"\u5FFD\u7565\u544A\u8B66",unCheckedChildren:"\u4E0D\u5904\u7F6E",onChange:function(P){T(P),p(P)}})})})]})]}):(0,s.jsx)("div",{className:"text-c",children:(0,s.jsx)(xe.ZP.Group,{defaultValue:"update",value:H,options:[{label:"\u5FFD\u7565\u9009\u4E2D\u7684\u544A\u8B66\uFF08\u4EC5\u4EC5\u5FFD\u7565\u6B64\u6761\u544A\u8B66\u4FE1\u606F\uFF09",value:"update"},{label:"\u5FFD\u7565\u9009\u4E2D\u7684\u544A\u8B66\u5E76\u66F4\u65B0\u753B\u50CF",value:"delete"}],onChange:function(P){var _=P.target.value;M(_)}})})})}var Xe=(0,t.forwardRef)(Je),Ve=e(25377),ke=e(93488),qe=function(n){var r=(0,t.useState)([]),i=(0,v.Z)(r,2),l=i[0],d=i[1],c=(0,t.useState)(0),D=(0,v.Z)(c,2),A=D[0],T=D[1],C=(0,t.useState)(!1),x=(0,v.Z)(C,2),Y=x[0],p=x[1],f=(0,t.useState)(!1),w=(0,v.Z)(f,2),J=w[0],W=w[1],ne=(0,t.useState)([]),G=(0,v.Z)(ne,2),m=G[0],ie=G[1],X=(0,Ve.md)(),ae=(0,t.useState)({pageNo:1,pageSize:10}),H=(0,v.Z)(ae,2),M=H[0],V=H[1],P=(0,t.useRef)(),_=(0,t.useState)({}),le=(0,v.Z)(_,2),$e=le[0],tt=le[1],He=function(j){ie(j)},nt={selectedRowKeys:m,onChange:He,getCheckboxProps:function(j){return(0,N.Z)((0,N.Z)({},j),{},{disabled:j.matchRuleId===3})}},at=m.length>0,Ie=(0,t.useCallback)(function(B){He([]),(0,I.cD)(B).then(function(j){var S=j.data;if(p(!0),S){var k=JSON.parse(S);if(T(k.total),k){var q=JSON.parse(k.rows);d(q.filter(function(Qe){return Qe.isDelete===0}))}else d([])}p(!1)})},[]),lt=[{title:"\u8BB0\u5F55\u4EA7\u751F\u7684\u65F6\u95F4",dataIndex:"originalTime\uFF1A",key:"originalTime\uFF1A",width:"250px",align:"center",render:function(j,S){return S.originalTime||"-"}},{title:"\u89C4\u5219ID",dataIndex:"matchRuleId\uFF1A",key:"matchRuleId\uFF1A",width:"250px",align:"center",render:function(j,S){return S.matchRuleId||"-"}},{title:"\u544A\u8B66\u5185\u5BB9",dataIndex:"alarmContent",key:"alarmContent",align:"center"},{title:"\u5904\u7F6E",width:"150px",align:"center",render:function(j,S){return[(0,s.jsx)(Ve.Nv,{accessible:X.canUpdate,children:S.matchRuleId!==3?(0,s.jsx)("a",{onClick:function(){var q;W(!0),(q=P.current)===null||q===void 0||q.setInitValues([S])},children:"\u5904\u7F6E"}):"-"},"handle")]}}];return(0,t.useEffect)(function(){var B=n.location.query;tt(B),Ie((0,N.Z)((0,N.Z)((0,N.Z)({},B),M),{isDelete:0}))},[Ie,M,n.location.query]),(0,s.jsxs)("div",{className:b().alarmDetail,children:[(0,s.jsx)(L.Z,{spinning:Y,children:(0,s.jsx)(O.Z,{rowSelection:nt,columns:lt,dataSource:l,rowKey:"id",title:function(){return(0,s.jsxs)("div",{children:[(0,s.jsxs)("div",{children:[(0,s.jsx)("h3",{className:"ib",children:$e.userName}),(0,s.jsxs)("h3",{className:"ib ml-10",children:["\u89E6\u53D1\u4E86 ",(0,s.jsx)("span",{className:"danger",children:A})," \u6761\u544A\u8B66"]})]}),(0,s.jsxs)("div",{className:"flex-x-sb",children:[(0,s.jsx)(Ve.Nv,{accessible:X.canUpdate,children:(0,s.jsx)(Q.Z,{type:"primary",disabled:!at,size:"middle",onClick:function(){var S,k=l.filter(function(q){return m.includes(q.id)});W(!0),(S=P.current)===null||S===void 0||S.setInitValues(k)},children:"\u6279\u91CF\u5904\u7F6E"})}),(0,s.jsxs)(Q.Z,{type:"primary",onClick:function(){n.history.goBack()},children:["\u8FD4 \u56DE",(0,s.jsx)(ke.Z,{})]})]})]})},size:"middle",pagination:{showSizeChanger:!1,pageSize:M.pageSize,current:M.pageNo,total:A,onChange:function(){var B=(0,U.Z)(se().mark(function S(k,q){return se().wrap(function(Re){for(;;)switch(Re.prev=Re.next){case 0:return Re.next=2,V(function(){return{pageSize:q,pageNo:k}});case 2:Ie((0,N.Z)({pageNo:k,pageSize:q},$e));case 3:case"end":return Re.stop()}},S)}));function j(S,k){return B.apply(this,arguments)}return j}()}})}),(0,s.jsx)(Xe,{refresh:function(){Ie((0,N.Z)((0,N.Z)({},M),$e))},setVisible:W,handleVisible:J,ref:P})]})},et=qe},48606:function(me,ee,e){"use strict";e.d(ee,{Hp:function(){return re},CI:function(){return N},cD:function(){return ce},QD:function(){return t}});var Z=e(11849),L=e(3182),E=e(94043),O=e.n(E),U=e(25377);function re(b){return Q.apply(this,arguments)}function Q(){return Q=(0,L.Z)(O().mark(function b(I){return O().wrap(function(R){for(;;)switch(R.prev=R.next){case 0:return R.abrupt("return",(0,U.WY)("/api/skyflying/getAlarmData",(0,Z.Z)({method:"GET"},I||{})));case 1:case"end":return R.stop()}},b)})),Q.apply(this,arguments)}function N(b){return v.apply(this,arguments)}function v(){return v=(0,L.Z)(O().mark(function b(I){return O().wrap(function(R){for(;;)switch(R.prev=R.next){case 0:return R.abrupt("return",(0,U.WY)("/api/skyflying/getUserAlarmData",(0,Z.Z)({method:"GET"},I||{})));case 1:case"end":return R.stop()}},b)})),v.apply(this,arguments)}function ce(b,I){return se.apply(this,arguments)}function se(){return se=(0,L.Z)(O().mark(function b(I,oe){return O().wrap(function(K){for(;;)switch(K.prev=K.next){case 0:return K.abrupt("return",(0,U.WY)("/api/skyflying/getAllAlarmInfoDetailByUserName",(0,Z.Z)({method:"GET",params:I||{}},oe||{})));case 1:case"end":return K.stop()}},b)})),se.apply(this,arguments)}function t(b,I){return ue.apply(this,arguments)}function ue(){return ue=(0,L.Z)(O().mark(function b(I,oe){return O().wrap(function(K){for(;;)switch(K.prev=K.next){case 0:return K.abrupt("return",(0,U.WY)("/api/skyflying/updateAnomalyDetectionInfo",(0,Z.Z)({method:"POST",headers:{"Content-Type":"application/json;charset=UTF-8;"},data:JSON.stringify(I||{})},oe||{})));case 1:case"end":return K.stop()}},b)})),ue.apply(this,arguments)}},75443:function(me,ee,e){"use strict";var Z=e(22122),L=e(28481),E=e(67294),O=e(94184),U=e.n(O),re=e(21770),Q=e(68855),N=e(15105),v=e(61580),ce=e(71577),se=e(73839),t=e(42051),ue=e(85636),b=e(65632),I=e(81643),oe=e(96159),R=e(33603),K=e(86743),xe=void 0,ze=function(o,ve){var fe={};for(var g in o)Object.prototype.hasOwnProperty.call(o,g)&&ve.indexOf(g)<0&&(fe[g]=o[g]);if(o!=null&&typeof Object.getOwnPropertySymbols=="function")for(var te=0,g=Object.getOwnPropertySymbols(o);te<g.length;te++)ve.indexOf(g[te])<0&&Object.prototype.propertyIsEnumerable.call(o,g[te])&&(fe[g[te]]=o[g[te]]);return fe},pe=E.forwardRef(function(o,ve){var fe=E.useContext(b.E_),g=fe.getPrefixCls,te=(0,re.Z)(!1,{value:o.visible,defaultValue:o.defaultVisible}),Pe=(0,L.Z)(te,2),Ae=Pe[0],we=Pe[1],$=function(u,h){var F;we(u,!0),(F=o.onVisibleChange)===null||F===void 0||F.call(o,u,h)},Me=function(u){$(!1,u)},Se=function(u){var h;return(h=o.onConfirm)===null||h===void 0?void 0:h.call(xe,u)},Te=function(u){var h;$(!1,u),(h=o.onCancel)===null||h===void 0||h.call(xe,u)},de=function(u){u.keyCode===N.Z.ESC&&Ae&&$(!1,u)},Ne=function(u){var h=o.disabled;h||$(u)},he=function(u,h){var F=o.okButtonProps,Ke=o.cancelButtonProps,Oe=o.title,Fe=o.cancelText,be=o.okText,ge=o.okType,Ge=o.icon,De=o.showCancel,We=De===void 0?!0:De;return E.createElement("div",{className:"".concat(u,"-inner-content")},E.createElement("div",{className:"".concat(u,"-message")},Ge,E.createElement("div",{className:"".concat(u,"-message-title")},(0,I.Z)(Oe))),E.createElement("div",{className:"".concat(u,"-buttons")},We&&E.createElement(ce.Z,(0,Z.Z)({onClick:Te,size:"small"},Ke),Fe||h.cancelText),E.createElement(K.Z,{buttonProps:(0,Z.Z)((0,Z.Z)({size:"small"},(0,se.n)(ge)),F),actionFn:Se,close:Me,prefixCls:g("btn"),quitOnNullishReturnValue:!0,emitEvent:!0},be||h.okText)))},Ze=o.prefixCls,_e=o.placement,y=o.children,ye=o.overlayClassName,Be=ze(o,["prefixCls","placement","children","overlayClassName"]),Ce=g("popover",Ze),Ee=g("popconfirm",Ze),je=U()(Ee,ye),Le=E.createElement(t.Z,{componentName:"Popconfirm",defaultLocale:ue.Z.Popconfirm},function(z){return he(Ce,z)}),Ue=g();return E.createElement(v.Z,(0,Z.Z)({},Be,{prefixCls:Ce,placement:_e,onVisibleChange:Ne,visible:Ae,overlay:Le,overlayClassName:je,ref:ve,transitionName:(0,R.mL)(Ue,"zoom-big",o.transitionName)}),(0,oe.Tm)(y,{onKeyDown:function(u){var h,F;E.isValidElement(y)&&((F=y==null?void 0:(h=y.props).onKeyDown)===null||F===void 0||F.call(h,u)),de(u)}}))});pe.defaultProps={placement:"top",trigger:"click",okType:"primary",icon:E.createElement(Q.Z,null),disabled:!1},ee.Z=pe},62350:function(me,ee,e){"use strict";var Z=e(38663),L=e.n(Z),E=e(20136),O=e(57663),U=e(44887),re=e.n(U)}}]);
