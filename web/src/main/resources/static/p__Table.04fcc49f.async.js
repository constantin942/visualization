(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[310],{97796:function(R){R.exports={table:"table___3lKck"}},24927:function(R,V,e){"use strict";e.r(V),e.d(V,{default:function(){return ne}});var de=e(62350),O=e(75443),ce=e(74379),U=e(38648),a=e(11849),b=e(2824),r=e(67294),W=e(97796),Y=e.n(W),fe=e(71194),J=e(50146),ve=e(57663),K=e(71577),D=e(3182),H=e(94043),c=e.n(H),I=e(5894),L=e(5966),S=e(25377),Q=e(80129),M=e.n(Q);function X(n,l){return j.apply(this,arguments)}function j(){return j=(0,D.Z)(c().mark(function n(l,u){return c().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,S.WY)("/api/skyflying/getAllMonitorTables",(0,a.Z)({method:"GET",params:l},u||{})));case 1:case"end":return t.stop()}},n)})),j.apply(this,arguments)}function w(n,l){return E.apply(this,arguments)}function E(){return E=(0,D.Z)(c().mark(function n(l,u){return c().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,S.WY)("/api/skyflying/updateMonitorTable?"+M().stringify(l),(0,a.Z)({method:"POST"},u||{})));case 1:case"end":return t.stop()}},n)})),E.apply(this,arguments)}function k(n,l){return A.apply(this,arguments)}function A(){return A=(0,D.Z)(c().mark(function n(l,u){return c().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,S.WY)("/api/skyflying/updateMonitorTableDesc?"+M().stringify(l),(0,a.Z)({method:"POST"},u||{})));case 1:case"end":return t.stop()}},n)})),A.apply(this,arguments)}var s=e(85893);function q(n,l){var u={labelCol:{span:6},wrapperCol:{span:20}},Z=(0,r.useState)(),t=(0,b.Z)(Z,2),f=t[0],N=t[1],C=(0,r.useRef)();return(0,r.useImperativeHandle)(l,function(){return{setInitValues:function(p){var v;N(p),(v=C.current)===null||v===void 0||v.setFieldsValue({tableDesc:p.tableDesc})}}}),(0,s.jsx)(J.Z,{width:400,title:"\u4FEE\u6539\u63CF\u8FF0",visible:n.visible,footer:[],onCancel:function(){return n.setVisible(!1)},children:(0,s.jsxs)(I.A,(0,a.Z)((0,a.Z)({formRef:C,layout:"horizontal"},u),{},{onFinish:function(){var y=(0,D.Z)(c().mark(function p(v){return c().wrap(function(F){for(;;)switch(F.prev=F.next){case 0:k({id:f==null?void 0:f.id,tableDesc:v.tableDesc,tableName:f==null?void 0:f.tableName}).then(function(){n.refresh(),n.setVisible(!1)});case 1:case"end":return F.stop()}},p)}));return function(p){return y.apply(this,arguments)}}(),submitter:!1,children:[(0,s.jsx)(L.Z,{fieldProps:{placeholder:"\u8BF7\u8F93\u5165\u8868\u7684\u63CF\u8FF0\u6587\u5B57"},label:"\u63CF\u8FF0\u6587\u5B57",rules:[{required:!0,message:"\u8BF7\u586B\u5199\u63CF\u8FF0\u6587\u5B57"}],name:"tableDesc"}),(0,s.jsx)(I.A.Group,{style:{textAlign:"center"},children:(0,s.jsx)(K.Z,{type:"primary",htmlType:"submit",className:"w-100",children:"\u786E\u8BA4\u4FEE\u6539"})})]}))})}var _=(0,r.forwardRef)(q),ee=e(83539),te=e(13312),ae=function(){var l=(0,r.useState)([]),u=(0,b.Z)(l,2),Z=u[0],t=u[1],f=(0,r.useState)(0),N=(0,b.Z)(f,2),C=N[0],y=N[1],p=(0,r.useState)({}),v=(0,b.Z)(p,2),x=v[0],F=v[1],se=(0,r.useState)(!1),P=(0,b.Z)(se,2),re=P[0],z=P[1],$=(0,r.useRef)(),B=(0,S.md)(),le=(0,r.useState)({pageNo:1,pageSize:10}),G=(0,b.Z)(le,2),m=G[0],ie=G[1],h=(0,r.useCallback)(function(T){X(T).then(function(g){var i=g.data,o=JSON.parse(i);if(o&&o.rows){var d=JSON.parse(o.rows);if(d&&d.length>0){y(o.total),t(d);return}}y(0),t([])})},[]);(0,r.useEffect)(function(){h((0,a.Z)({},m))},[h,m]);var ue=[{title:"\u6570\u636E\u5E93\u5730\u5740",dataIndex:"dbAddress",align:"center"},{title:"\u6570\u636E\u5E93\u540D",dataIndex:"dbName",align:"center",ellipsis:!0},{title:"\u8868\u540D",dataIndex:"tableName",align:"center"},{title:"\u63CF\u8FF0",dataIndex:"tableDesc",align:"center"},{title:"\u72B6\u6001",align:"center",dataIndex:"isDelete",search:!1,initialValue:"0",valueEnum:{0:{text:"\u542F\u7528\u4E2D",status:"Success"},1:{text:"\u5DF2\u7981\u7528",status:"Error"}}},{title:"\u66F4\u65B0\u65F6\u95F4",key:"gmtModified",dataIndex:"gmtModified",align:"center",valueType:"dateTime",search:!1},{title:"\u64CD\u4F5C",key:"option",align:"center",valueType:"option",render:function(g,i){return[(0,s.jsx)(S.Nv,{accessible:B.canUpdate,children:(0,s.jsx)(O.Z,{title:"\u60A8\u786E\u8BA4".concat(i.isDelete===0?"\u7981\u7528":"\u542F\u7528").concat(i.tableName,"\u8868\u5417\uFF1F"),placement:"top",onConfirm:function(){w({id:i.id,isDelete:i.isDelete===1?0:1,tableName:i.tableName}).then(function(d){var oe=d.code;oe===te.Gh&&(U.Z.success({message:"\u66F4\u65B0\u6210\u529F\uFF01"}),h((0,a.Z)((0,a.Z)({},m),x)))})},children:(0,s.jsx)("a",{className:i.isDelete===0?"danger":"",children:i.isDelete===0?"\u7981\u7528":"\u542F\u7528"})})},"link"),(0,s.jsx)(S.Nv,{accessible:B.canUpdate,children:(0,s.jsx)("a",{onClick:function(){var d;(d=$.current)===null||d===void 0||d.setInitValues(i),z(!0)},children:"\u66F4\u65B0"})},"update")]}}];return(0,s.jsxs)("div",{className:Y().table,children:[(0,s.jsx)(ee.ZP,{columns:ue,dataSource:Z,rowKey:"id",pagination:{showSizeChanger:!1,total:C,pageSize:m.pageSize,current:m.pageNo,onChange:function(g,i){var o={pageNo:g,pageSize:i};ie(o),h((0,a.Z)((0,a.Z)({},o),x))}},search:{},onSubmit:function(g){F(g),h((0,a.Z)((0,a.Z)({},m),g))},onReset:function(){h((0,a.Z)((0,a.Z)({},m),x))},dateFormatter:"string",headerTitle:"\u5DF2\u6709\u6570\u636E\u5E93\u8868",toolBarRender:!1}),(0,s.jsx)(_,{ref:$,visible:re,setVisible:z,refresh:function(){h((0,a.Z)((0,a.Z)({},m),x))}})]})},ne=ae}}]);