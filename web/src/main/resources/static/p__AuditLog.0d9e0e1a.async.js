(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[504],{78844:function(Z,A,e){"use strict";var E=e(22122),i=e(28991),b=e(81253),a=e(67294),u=e(30885),S=e(51617),d=e(66758),U=["fieldProps","proFieldProps"],P="dateTimeRange",p=a.forwardRef(function(v,T){var f=v.fieldProps,O=v.proFieldProps,s=(0,b.Z)(v,U),g=(0,a.useContext)(d.Z);return a.createElement(u.Z,(0,E.Z)({ref:T,mode:"edit",fieldProps:(0,i.Z)({getPopupContainer:g.getPopupContainer},f),valueType:P,proFieldProps:O,filedConfig:{valueType:P,lightFilterLabelFormatter:function(C){return(0,S.Z)(C,"YYYY-MM-DD HH:mm:ss")}}},s))});A.Z=p},64317:function(Z,A,e){"use strict";var E=e(22122),i=e(28991),b=e(81253),a=e(67294),u=e(30885),S=e(96202),d=e(66758),U=["fieldProps","children","params","proFieldProps","mode","valueEnum","request","showSearch","options"],P=["fieldProps","children","params","proFieldProps","mode","valueEnum","request","options"],p=a.forwardRef(function(s,g){var D=s.fieldProps,C=s.children,t=s.params,_=s.proFieldProps,l=s.mode,c=s.valueEnum,r=s.request,m=s.showSearch,F=s.options,W=(0,b.Z)(s,U),L=(0,a.useContext)(d.Z);return a.createElement(u.Z,(0,E.Z)({mode:"edit",valueEnum:(0,S.h)(c),request:r,params:t,valueType:"select",filedConfig:{customLightMode:!0},fieldProps:(0,i.Z)({options:F,mode:l,showSearch:m,getPopupContainer:L.getPopupContainer},D),ref:g,proFieldProps:_},W),C)}),v=a.forwardRef(function(s,g){var D=s.fieldProps,C=s.children,t=s.params,_=s.proFieldProps,l=s.mode,c=s.valueEnum,r=s.request,m=s.options,F=(0,b.Z)(s,P),W=(0,i.Z)({options:m,mode:l||"multiple",labelInValue:!0,showSearch:!0,showArrow:!1,autoClearSearchValue:!0,optionLabelProp:"label"},D),L=(0,a.useContext)(d.Z);return a.createElement(u.Z,(0,E.Z)({mode:"edit",valueEnum:(0,S.h)(c),request:r,params:t,valueType:"select",filedConfig:{customLightMode:!0},fieldProps:(0,i.Z)({getPopupContainer:L.getPopupContainer},W),ref:g,proFieldProps:_},F),C)}),T=p,f=v,O=T;O.SearchSelect=f,O.displayName="ProFormComponent",A.Z=O},59712:function(Z){Z.exports={auditLog:"auditLog___oJOgF","auditLog__form-wrap":"auditLog__form-wrap___1CBpJ",content:"content___Hfc44"}},6227:function(Z,A,e){"use strict";e.r(A);var E=e(49111),i=e(19650),b=e(71153),a=e(60331),u=e(11849),S=e(3182),d=e(2824),U=e(94043),P=e.n(U),p=e(67294),v=e(59712),T=e.n(v),f=e(54154),O=e(89366),s=e(5894),g=e(64317),D=e(78844),C=e(29474),t=e(85893),_=function(){var c=(0,p.useState)([]),r=(0,d.Z)(c,2),m=r[0],F=r[1],W=(0,p.useState)([]),L=(0,d.Z)(W,2),z=L[0],G=L[1],$=(0,p.useState)([]),N=(0,d.Z)($,2),J=N[0],X=N[1],H=(0,p.useState)([]),j=(0,d.Z)(H,2),Q=j[0],x=j[1],V=(0,p.useRef)(),k=(0,p.useState)(0),K=(0,d.Z)(k,2),q=K[0],ee=K[1],te=(0,p.useState)({pageNo:1,pageSize:20}),Y=(0,d.Z)(te,2),M=Y[0],re=Y[1],I=function(o){(0,f.CX)(o).then(function(n){var y=n.data,R=JSON.parse(y);if(ee(R.total),R.rows){var B=JSON.parse(R.rows);x(B)}else x([])})};return(0,p.useEffect)(function(){(0,f.m8)().then(function(h){var o=h.data;if(o){var n=JSON.parse(o);F(n)}}),(0,f.LX)().then(function(h){var o=h.data;if(o){var n=JSON.parse(o);X(n)}}),(0,f.gY)().then(function(h){var o=h.data;if(o){var n=JSON.parse(o);G(n)}}),I(M)},[]),(0,t.jsxs)("div",{className:T().auditLog,children:[(0,t.jsx)("div",{className:T()["auditLog__form-wrap"],children:(0,t.jsxs)(s.A,{layout:"inline",formRef:V,size:"middle",onFinish:function(){var h=(0,S.Z)(P().mark(function o(n){var y;return P().wrap(function(B){for(;;)switch(B.prev=B.next){case 0:y={},Object.keys(n).length>0&&(n.times?(y=(0,u.Z)({startTime:n[0],endTime:n.times[1]},n),delete y.times):y=(0,u.Z)({},n),re(function(w){return(0,u.Z)({pageNo:w.pageNo,pageSize:w.pageSize},y)})),I((0,u.Z)({pageSize:M.pageSize,pageNo:M.pageNo},y));case 3:case"end":return B.stop()}},o)}));return function(o){return h.apply(this,arguments)}}(),submitter:{searchConfig:{submitText:"\u67E5\u8BE2"}},children:[(0,t.jsx)(s.A.Group,{children:(0,t.jsx)(g.Z,{options:m,placeholder:"\u7528\u6237\u540D",name:"dbUserName",filedConfig:{},fieldProps:{}},"value")}),(0,t.jsx)(s.A.Group,{children:(0,t.jsx)(g.Z,{options:z,width:"sm",placeholder:"\u6570\u636E\u5E93\u8868\u540D",name:"msTableName",filedConfig:{},fieldProps:{}},"value")}),(0,t.jsx)(s.A.Group,{children:(0,t.jsx)(g.Z,{options:J,placeholder:"\u64CD\u4F5C\u7C7B\u578B",name:"sqlType"},"value")}),(0,t.jsx)(s.A.Group,{children:(0,t.jsx)(D.Z,{width:"md",fieldProps:{placeholder:["\u5F00\u59CB\u65F6\u95F4","\u7ED3\u675F\u65F6\u95F4"],bordered:!0},name:"times"})})]})}),(0,t.jsx)("div",{className:T().content,children:(0,t.jsx)(C.Rs,{rowKey:"id",dataSource:Q,headerTitle:"\u6570\u636E\u5E93\u5BA1\u8BA1\u65E5\u5FD7\u5217\u8868",pagination:{showSizeChanger:!1,pageSize:M.pageSize,total:q,current:M.pageNo,size:"small",onChange:function(o,n){I({pageNo:o,pageSize:n,dbUserName:M.dbUserName,sqlType:M.sqlType,msTableName:M.msTableName,startTime:M.startTime,endTime:M.endTime})}},metas:{avatar:{render:function(){return(0,t.jsx)(O.Z,{style:{color:"#52c41a"}})},search:!1},title:{dataIndex:"userName",search:!1},description:{dataIndex:"instanceName",search:!1,render:function(o,n){return[(0,t.jsxs)("div",{className:"fs-12",children:[(0,t.jsx)("span",{children:"\u6570\u636E\u5E93\u5B9E\u4F8B\u5730\u5740\uFF1A"}),(0,t.jsx)("span",{style:{color:"#36cfc9"},children:n.instanceName})]},"instanceName")]}},subTitle:{search:!1,dataIndex:"opTime",render:function(o,n){return[(0,t.jsxs)(i.Z,{children:[(0,t.jsx)("span",{children:n.opTime},"opTime"),n.sqlType&&n.sqlType!=="UNKNOW"&&(0,t.jsx)(a.Z,{color:"#55acee",children:(0,t.jsxs)("span",{className:"fs-12",children:["\u64CD\u4F5C\u7C7B\u578B\uFF1A",n.sqlType]})},"sqlType"),n.msTableName&&(0,t.jsx)(a.Z,{color:"#55acee",children:(0,t.jsxs)("span",{className:"fs-12",children:["\u8868\u540D\uFF1A",n.msTableName]})},"sqlType")]},"space")]}},content:{dataIndex:"msSql",search:!1,render:function(o,n){return(0,t.jsxs)("div",{children:[(0,t.jsx)("div",{style:{color:"#00000073"},children:"SQL\u8BED\u53E5"}),(0,t.jsx)("div",{style:{color:"#000000D9"},children:n.msSql})]},"msSql")}}}})})]})};A.default=_},54154:function(Z,A,e){"use strict";e.d(A,{Y8:function(){return S},IY:function(){return U},z2:function(){return p},m8:function(){return T},LX:function(){return O},gY:function(){return g},CX:function(){return C}});var E=e(11849),i=e(3182),b=e(94043),a=e.n(b),u=e(32773);function S(_){return d.apply(this,arguments)}function d(){return d=(0,i.Z)(a().mark(function _(l){return a().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,u.WY)("/api/skyflying/getAllUserName",(0,E.Z)({method:"GET"},l||{})));case 1:case"end":return r.stop()}},_)})),d.apply(this,arguments)}function U(_){return P.apply(this,arguments)}function P(){return P=(0,i.Z)(a().mark(function _(l){return a().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,u.WY)("/api/skyflying/getAllInstanceAndTableName",(0,E.Z)({method:"GET"},l||{})));case 1:case"end":return r.stop()}},_)})),P.apply(this,arguments)}function p(_,l){return v.apply(this,arguments)}function v(){return v=(0,i.Z)(a().mark(function _(l,c){return a().wrap(function(m){for(;;)switch(m.prev=m.next){case 0:return m.abrupt("return",(0,u.WY)("/api/skyflying/getAllSegments",(0,E.Z)({method:"GET",timeout:6e4,headers:{"Content-Type":"application/json"},params:l},c||{})));case 1:case"end":return m.stop()}},_)})),v.apply(this,arguments)}function T(_){return f.apply(this,arguments)}function f(){return f=(0,i.Z)(a().mark(function _(l){return a().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,u.WY)("/api/skyflying/getAllUserNameFromDMS",(0,E.Z)({timeout:6e4},l||{})));case 1:case"end":return r.stop()}},_)})),f.apply(this,arguments)}function O(_){return s.apply(this,arguments)}function s(){return s=(0,i.Z)(a().mark(function _(l){return a().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,u.WY)("/api/skyflying/getAllSqlTypeFromDMS",(0,E.Z)({timeout:6e4},l||{})));case 1:case"end":return r.stop()}},_)})),s.apply(this,arguments)}function g(_){return D.apply(this,arguments)}function D(){return D=(0,i.Z)(a().mark(function _(l){return a().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,u.WY)("/api/skyflying/getAllTableNameFromDMS",(0,E.Z)({timeout:6e4},l||{})));case 1:case"end":return r.stop()}},_)})),D.apply(this,arguments)}function C(_,l){return t.apply(this,arguments)}function t(){return t=(0,i.Z)(a().mark(function _(l,c){return a().wrap(function(m){for(;;)switch(m.prev=m.next){case 0:return m.abrupt("return",(0,u.WY)("/api/skyflying/getDmsAuditLogFromDb",(0,E.Z)({method:"GET",timeout:6e4,params:l},c||{})));case 1:case"end":return m.stop()}},_)})),t.apply(this,arguments)}}}]);
