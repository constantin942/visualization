(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[504],{78844:function(b,S,e){"use strict";var C=e(22122),h=e(28991),U=e(81253),t=e(67294),u=e(30885),_=e(51617),f=e(66758),W=["fieldProps","proFieldProps"],T="dateTimeRange",d=t.forwardRef(function(m,y){var M=m.fieldProps,A=m.proFieldProps,E=(0,U.Z)(m,W),g=(0,t.useContext)(f.Z);return t.createElement(u.Z,(0,C.Z)({ref:y,mode:"edit",fieldProps:(0,h.Z)({getPopupContainer:g.getPopupContainer},M),valueType:T,proFieldProps:A,filedConfig:{valueType:T,lightFilterLabelFormatter:function(L){return(0,_.Z)(L,"YYYY-MM-DD HH:mm:ss")}}},E))});S.Z=d},5894:function(b,S,e){"use strict";e.d(S,{A:function(){return i}});var C=e(9715),h=e(86835),U=e(22122),t=e(67294),u=e(96156),_=e(49111),f=e(19650),W=e(84305),T=e(75901),d=e(28481),m=e(28991),y=e(8812),M=e(66758),A=e(96138),E=e(28664),g=e(58927),Z=e(94184),L=e.n(Z),n=e(2514),r=t.forwardRef(function(D,F){var B=t.useContext(M.Z),N=B.groupProps,p=(0,m.Z)((0,m.Z)({},N),D),w=p.children,j=p.collapsible,ae=p.defaultCollapsed,se=p.style,re=p.labelLayout,K=p.title,G=K===void 0?D.label:K,z=p.tooltip,k=p.align,q=k===void 0?"start":k,$=p.direction,ee=p.size,te=ee===void 0?32:ee,le=p.titleStyle,Y=p.titleRender,v=p.spaceProps,J=p.extra,I=p.autoFocus,P=(0,E.Z)(function(){return ae||!1},{value:D.collapsed,onChange:D.onCollapse}),o=(0,d.Z)(P,2),s=o[0],O=o[1],ne=(0,t.useContext)(T.ZP.ConfigContext),H=ne.getPrefixCls,X=(0,n.z)(D),pe=X.ColWrapper,ie=X.RowWrapper,V=H("pro-form-group"),ue=j&&t.createElement(y.Z,{style:{marginRight:8},rotate:s?void 0:90}),_e=t.createElement(g.Z,{label:ue?t.createElement("div",null,ue,G):G,tooltip:z}),de=(0,t.useCallback)(function(x){var Q=x.children;return t.createElement(f.Z,(0,U.Z)({},v,{className:"".concat(V,"-container"),size:te,align:q,direction:$,style:(0,m.Z)({rowGap:0},v==null?void 0:v.style)}),Q)},[q,V,$,te,v]),me=Y?Y(_e,D):_e,ve=(0,t.useMemo)(function(){var x=[],Q=t.Children.toArray(w).map(function(R,ge){var oe;return t.isValidElement(R)&&(R==null||(oe=R.props)===null||oe===void 0?void 0:oe.hidden)?(x.push(R),null):ge===0&&t.isValidElement(R)&&I?t.cloneElement(R,(0,m.Z)((0,m.Z)({},R.props),{},{autoFocus:I})):R});return[t.createElement(ie,{key:"children",Wrapper:de},Q),x.length>0?t.createElement("div",{style:{display:"none"}},x):null]},[w,ie,de,I]),ce=(0,d.Z)(ve,2),fe=ce[0],Ee=ce[1];return t.createElement(pe,null,t.createElement("div",{className:L()(V,(0,u.Z)({},"".concat(V,"-twoLine"),re==="twoLine")),style:se,ref:F},Ee,(G||z||J)&&t.createElement("div",{className:"".concat(V,"-title"),style:le,onClick:function(){O(!s)}},J?t.createElement("div",{style:{display:"flex",width:"100%",alignItems:"center",justifyContent:"space-between"}},me,t.createElement("span",{onClick:function(Q){return Q.stopPropagation()}},J)):me),j&&s?null:fe))});r.displayName="ProForm-Group";var l=r,c=e(87808),a=e(24214);function i(D){return t.createElement(a.I,(0,U.Z)({layout:"vertical",submitter:{render:function(B,N){return N.reverse()}},contentRender:function(B,N){return t.createElement(t.Fragment,null,B,N)}},D))}i.Group=l,i.useForm=h.Z.useForm,i.Item=c.Z},58927:function(b,S,e){"use strict";var C=e(22385),h=e(61580),U=e(96156),t=e(84305),u=e(75901),_=e(67294),f=e(68628),W=e(11445),T=e.n(W),d=e(94184),m=e.n(d),y=function(A){var E=A.label,g=A.tooltip,Z=A.ellipsis,L=A.subTitle,n=(0,_.useContext)(u.ZP.ConfigContext),r=n.getPrefixCls;if(!g&&!L)return _.createElement(_.Fragment,null,E);var l=r("pro-core-label-tip"),c=typeof g=="string"||_.isValidElement(g)?{title:g}:g,a=(c==null?void 0:c.icon)||_.createElement(f.Z,null);return _.createElement("div",{className:l,onMouseDown:function(D){return D.stopPropagation()},onMouseLeave:function(D){return D.stopPropagation()},onMouseMove:function(D){return D.stopPropagation()}},_.createElement("div",{className:m()("".concat(l,"-title"),(0,U.Z)({},"".concat(l,"-title-ellipsis"),Z))},E),L&&_.createElement("div",{className:"".concat(l,"-subtitle")},L),g&&_.createElement(h.Z,c,_.createElement("span",{className:"".concat(l,"-icon")},a)))};S.Z=_.memo(y)},59712:function(b){b.exports={auditLog:"auditLog___oJOgF","auditLog__form-wrap":"auditLog__form-wrap___1CBpJ",content:"content___Hfc44"}},96138:function(){},11445:function(){},6227:function(b,S,e){"use strict";e.r(S);var C=e(49111),h=e(19650),U=e(71153),t=e(60331),u=e(11849),_=e(3182),f=e(2824),W=e(94043),T=e.n(W),d=e(67294),m=e(59712),y=e.n(m),M=e(54154),A=e(89366),E=e(5894),g=e(64317),Z=e(78844),L=e(29474),n=e(85893),r=function(){var c=(0,d.useState)([]),a=(0,f.Z)(c,2),i=a[0],D=a[1],F=(0,d.useState)([]),B=(0,f.Z)(F,2),N=B[0],p=B[1],w=(0,d.useState)([]),j=(0,f.Z)(w,2),ae=j[0],se=j[1],re=(0,d.useState)([]),K=(0,f.Z)(re,2),G=K[0],z=K[1],k=(0,d.useRef)(),q=(0,d.useState)(0),$=(0,f.Z)(q,2),ee=$[0],te=$[1],le=(0,d.useState)({pageNo:1,pageSize:20}),Y=(0,f.Z)(le,2),v=Y[0],J=Y[1],I=(0,d.useCallback)(function(P){(0,M.CX)(P).then(function(o){var s=o.data,O=JSON.parse(s);if(te(O.total),O.rows){var ne=JSON.parse(O.rows);z(ne)}else z([])})},[]);return(0,d.useEffect)(function(){(0,M.m8)().then(function(P){var o=P.data;if(o){var s=JSON.parse(o);D(s)}}),(0,M.LX)().then(function(P){var o=P.data;if(o){var s=JSON.parse(o);se(s)}}),(0,M.gY)().then(function(P){var o=P.data;if(o){var s=JSON.parse(o);p(s)}}),I(v)},[v,I]),(0,n.jsxs)("div",{className:y().auditLog,children:[(0,n.jsx)("div",{className:y()["auditLog__form-wrap"],children:(0,n.jsxs)(E.A,{layout:"inline",formRef:k,size:"middle",onFinish:function(){var P=(0,_.Z)(T().mark(function o(s){var O;return T().wrap(function(H){for(;;)switch(H.prev=H.next){case 0:O={},Object.keys(s).length>0&&(s.times?(O=(0,u.Z)({startTime:s[0],endTime:s.times[1]},s),delete O.times):O=(0,u.Z)({},s),J(function(X){return(0,u.Z)({pageNo:X.pageNo,pageSize:X.pageSize},O)})),I((0,u.Z)({pageSize:v.pageSize,pageNo:v.pageNo},O));case 3:case"end":return H.stop()}},o)}));return function(o){return P.apply(this,arguments)}}(),submitter:{searchConfig:{submitText:"\u67E5\u8BE2"}},children:[(0,n.jsx)(E.A.Group,{children:(0,n.jsx)(g.Z,{options:i,placeholder:"\u7528\u6237\u540D",name:"dbUserName",filedConfig:{},fieldProps:{}},"value")}),(0,n.jsx)(E.A.Group,{children:(0,n.jsx)(g.Z,{options:N,width:"sm",placeholder:"\u6570\u636E\u5E93\u8868\u540D",name:"msTableName",filedConfig:{},fieldProps:{}},"value")}),(0,n.jsx)(E.A.Group,{children:(0,n.jsx)(g.Z,{options:ae,placeholder:"\u64CD\u4F5C\u7C7B\u578B",name:"sqlType"},"value")}),(0,n.jsx)(E.A.Group,{children:(0,n.jsx)(Z.Z,{width:"md",fieldProps:{placeholder:["\u5F00\u59CB\u65F6\u95F4","\u7ED3\u675F\u65F6\u95F4"],bordered:!0},name:"times"})})]})}),(0,n.jsx)("div",{className:y().content,children:(0,n.jsx)(L.Rs,{rowKey:"id",dataSource:G,headerTitle:"\u6570\u636E\u5E93\u5BA1\u8BA1\u65E5\u5FD7\u5217\u8868",pagination:{showSizeChanger:!1,pageSize:v.pageSize,total:ee,current:v.pageNo,size:"small",onChange:function(o,s){I({pageNo:o,pageSize:s,dbUserName:v.dbUserName,sqlType:v.sqlType,msTableName:v.msTableName,startTime:v.startTime,endTime:v.endTime})}},metas:{avatar:{render:function(){return(0,n.jsx)(A.Z,{style:{color:"#52c41a"}})},search:!1},title:{dataIndex:"userName",search:!1},description:{dataIndex:"instanceName",search:!1,render:function(o,s){return[(0,n.jsxs)("div",{className:"fs-12",children:[(0,n.jsx)("span",{children:"\u6570\u636E\u5E93\u5B9E\u4F8B\u5730\u5740\uFF1A"}),(0,n.jsx)("span",{style:{color:"#36cfc9"},children:s.instanceName})]},"instanceName")]}},subTitle:{search:!1,dataIndex:"opTime",render:function(o,s){return[(0,n.jsxs)(h.Z,{children:[(0,n.jsx)("span",{children:s.opTime},"opTime"),s.sqlType&&s.sqlType!=="UNKNOW"&&(0,n.jsx)(t.Z,{color:"#55acee",children:(0,n.jsxs)("span",{className:"fs-12",children:["\u64CD\u4F5C\u7C7B\u578B\uFF1A",s.sqlType]})},"sqlType"),s.msTableName&&(0,n.jsx)(t.Z,{color:"#55acee",children:(0,n.jsxs)("span",{className:"fs-12",children:["\u8868\u540D\uFF1A",s.msTableName]})},"sqlType")]},"space")]}},content:{dataIndex:"msSql",search:!1,render:function(o,s){return(0,n.jsxs)("div",{children:[(0,n.jsx)("div",{style:{color:"#00000073"},children:"SQL\u8BED\u53E5"}),(0,n.jsx)("div",{style:{color:"#000000D9"},children:s.msSql})]},"msSql")}}}})})]})};S.default=r},54154:function(b,S,e){"use strict";e.d(S,{Y8:function(){return _},IY:function(){return W},z2:function(){return d},m8:function(){return y},LX:function(){return A},gY:function(){return g},CX:function(){return L}});var C=e(11849),h=e(3182),U=e(94043),t=e.n(U),u=e(25377);function _(r){return f.apply(this,arguments)}function f(){return f=(0,h.Z)(t().mark(function r(l){return t().wrap(function(a){for(;;)switch(a.prev=a.next){case 0:return a.abrupt("return",(0,u.WY)("/api/skyflying/getAllUserName",(0,C.Z)({method:"GET"},l||{})));case 1:case"end":return a.stop()}},r)})),f.apply(this,arguments)}function W(r){return T.apply(this,arguments)}function T(){return T=(0,h.Z)(t().mark(function r(l){return t().wrap(function(a){for(;;)switch(a.prev=a.next){case 0:return a.abrupt("return",(0,u.WY)("/api/skyflying/getAllInstanceAndTableName",(0,C.Z)({method:"GET"},l||{})));case 1:case"end":return a.stop()}},r)})),T.apply(this,arguments)}function d(r,l){return m.apply(this,arguments)}function m(){return m=(0,h.Z)(t().mark(function r(l,c){return t().wrap(function(i){for(;;)switch(i.prev=i.next){case 0:return i.abrupt("return",(0,u.WY)("/api/skyflying/getAllSegments",(0,C.Z)({method:"GET",timeout:6e4,headers:{"Content-Type":"application/json"},params:l},c||{})));case 1:case"end":return i.stop()}},r)})),m.apply(this,arguments)}function y(r){return M.apply(this,arguments)}function M(){return M=(0,h.Z)(t().mark(function r(l){return t().wrap(function(a){for(;;)switch(a.prev=a.next){case 0:return a.abrupt("return",(0,u.WY)("/api/skyflying/getAllUserNameFromDMS",(0,C.Z)({timeout:6e4},l||{})));case 1:case"end":return a.stop()}},r)})),M.apply(this,arguments)}function A(r){return E.apply(this,arguments)}function E(){return E=(0,h.Z)(t().mark(function r(l){return t().wrap(function(a){for(;;)switch(a.prev=a.next){case 0:return a.abrupt("return",(0,u.WY)("/api/skyflying/getAllSqlTypeFromDMS",(0,C.Z)({timeout:6e4},l||{})));case 1:case"end":return a.stop()}},r)})),E.apply(this,arguments)}function g(r){return Z.apply(this,arguments)}function Z(){return Z=(0,h.Z)(t().mark(function r(l){return t().wrap(function(a){for(;;)switch(a.prev=a.next){case 0:return a.abrupt("return",(0,u.WY)("/api/skyflying/getAllTableNameFromDMS",(0,C.Z)({timeout:6e4},l||{})));case 1:case"end":return a.stop()}},r)})),Z.apply(this,arguments)}function L(r,l){return n.apply(this,arguments)}function n(){return n=(0,h.Z)(t().mark(function r(l,c){return t().wrap(function(i){for(;;)switch(i.prev=i.next){case 0:return i.abrupt("return",(0,u.WY)("/api/skyflying/getDmsAuditLogFromDb",(0,C.Z)({method:"GET",timeout:6e4,params:l},c||{})));case 1:case"end":return i.stop()}},r)})),n.apply(this,arguments)}}}]);