(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[310],{97796:function(V){V.exports={table:"table___3lKck"}},24927:function(V,I,e){"use strict";e.r(I),e.d(I,{default:function(){return ne}});var F=e(3182),ce=e(62350),O=e(75443),de=e(74379),U=e(38648),s=e(11849),b=e(2824),W=e(94043),u=e.n(W),l=e(67294),Y=e(97796),J=e.n(Y),fe=e(71194),K=e(50146),ve=e(57663),H=e(71577),R=e(5894),L=e(5966),S=e(25377),Q=e(80129),M=e.n(Q);function X(n,i){return j.apply(this,arguments)}function j(){return j=(0,F.Z)(u().mark(function n(i,o){return u().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,S.WY)("/api/skyflying/getAllMonitorTables",(0,s.Z)({method:"GET",params:i},o||{})));case 1:case"end":return t.stop()}},n)})),j.apply(this,arguments)}function w(n,i){return E.apply(this,arguments)}function E(){return E=(0,F.Z)(u().mark(function n(i,o){return u().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,S.WY)("/api/skyflying/updateMonitorTable?"+M().stringify(i),(0,s.Z)({method:"POST"},o||{})));case 1:case"end":return t.stop()}},n)})),E.apply(this,arguments)}function k(n,i){return A.apply(this,arguments)}function A(){return A=(0,F.Z)(u().mark(function n(i,o){return u().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,S.WY)("/api/skyflying/updateMonitorTableDesc?"+M().stringify(i),(0,s.Z)({method:"POST"},o||{})));case 1:case"end":return t.stop()}},n)})),A.apply(this,arguments)}var r=e(85893);function q(n,i){var o={labelCol:{span:6},wrapperCol:{span:20}},y=(0,l.useState)(),t=(0,b.Z)(y,2),f=t[0],N=t[1],D=(0,l.useRef)();return(0,l.useImperativeHandle)(i,function(){return{setInitValues:function(m){var v;N(m),(v=D.current)===null||v===void 0||v.setFieldsValue({tableDesc:m.tableDesc})}}}),(0,r.jsx)(K.Z,{width:400,title:"\u4FEE\u6539\u63CF\u8FF0",visible:n.visible,footer:[],onCancel:function(){return n.setVisible(!1)},children:(0,r.jsxs)(R.A,(0,s.Z)((0,s.Z)({formRef:D,layout:"horizontal"},o),{},{onFinish:function(){var T=(0,F.Z)(u().mark(function m(v){return u().wrap(function(Z){for(;;)switch(Z.prev=Z.next){case 0:k({id:f==null?void 0:f.id,tableDesc:v.tableDesc,tableName:f==null?void 0:f.tableName}).then(function(){n.refresh(),n.setVisible(!1)});case 1:case"end":return Z.stop()}},m)}));return function(m){return T.apply(this,arguments)}}(),submitter:!1,children:[(0,r.jsx)(L.Z,{fieldProps:{placeholder:"\u8BF7\u8F93\u5165\u8868\u7684\u63CF\u8FF0\u6587\u5B57"},label:"\u63CF\u8FF0\u6587\u5B57",rules:[{required:!0,message:"\u8BF7\u586B\u5199\u63CF\u8FF0\u6587\u5B57"}],name:"tableDesc"}),(0,r.jsx)(R.A.Group,{style:{textAlign:"center"},children:(0,r.jsx)(H.Z,{type:"primary",htmlType:"submit",className:"w-100",children:"\u786E\u8BA4\u4FEE\u6539"})})]}))})}var _=(0,l.forwardRef)(q),ee=e(60355),te=e(13312),ae=function(){var i=(0,l.useState)([]),o=(0,b.Z)(i,2),y=o[0],t=o[1],f=(0,l.useState)(0),N=(0,b.Z)(f,2),D=N[0],T=N[1],m=(0,l.useState)({}),v=(0,b.Z)(m,2),C=v[0],Z=v[1],se=(0,l.useState)(!1),z=(0,b.Z)(se,2),re=z[0],P=z[1],$=(0,l.useRef)(),B=(0,S.md)(),le=(0,l.useState)({pageNo:1,pageSize:10}),G=(0,b.Z)(le,2),x=G[0],ie=G[1],g=(0,l.useCallback)(function(h){X(h).then(function(p){var a=p.data,c=JSON.parse(a);if(c&&c.rows){var d=JSON.parse(c.rows);if(d&&d.length>0){T(c.total),t(d);return}}T(0),t([])})},[]);(0,l.useEffect)(function(){g((0,s.Z)({},x))},[g,x]);var ue=[{title:"\u6570\u636E\u5E93\u5730\u5740",dataIndex:"dbAddress",align:"center"},{title:"\u6570\u636E\u5E93\u540D",dataIndex:"dbName",align:"center",ellipsis:!0},{title:"\u8868\u540D",dataIndex:"tableName",align:"center"},{title:"\u63CF\u8FF0",dataIndex:"tableDesc",align:"center"},{title:"\u72B6\u6001",align:"center",dataIndex:"isDelete",search:!1,initialValue:"0",valueEnum:{0:{text:"\u542F\u7528\u4E2D",status:"Success"},1:{text:"\u5DF2\u7981\u7528",status:"Error"}}},{title:"\u66F4\u65B0\u65F6\u95F4",key:"gmtModified",dataIndex:"gmtModified",align:"center",valueType:"dateTime",search:!1},{title:"\u64CD\u4F5C",key:"option",align:"center",valueType:"option",render:function(p,a){return[(0,r.jsx)(S.Nv,{accessible:B.canUpdate,children:(0,r.jsx)(O.Z,{title:"\u60A8\u786E\u8BA4".concat(a.isDelete===0?"\u7981\u7528":"\u542F\u7528").concat(a.tableName,"\u8868\u5417\uFF1F"),placement:"top",onConfirm:function(){w({id:a.id,isDelete:a.isDelete===1?0:1,tableName:a.tableName}).then(function(d){var oe=d.code;oe===te.Gh&&(U.Z.success({message:"".concat(a.isDelete===0?"\u7981\u7528":"\u542F\u7528","\u6210\u529F\uFF01")}),g((0,s.Z)((0,s.Z)({},x),C)))})},children:(0,r.jsx)("a",{className:a.isDelete===0?"danger":"",children:a.isDelete===0?"\u7981\u7528":"\u542F\u7528"})})},"link"),(0,r.jsx)(S.Nv,{accessible:B.canUpdate,children:(0,r.jsx)("a",{onClick:function(){var d;(d=$.current)===null||d===void 0||d.setInitValues(a),P(!0)},children:"\u66F4\u65B0"})},"update")]}}];return(0,r.jsxs)("div",{className:J().table,children:[(0,r.jsx)(ee.ZP,{columns:ue,dataSource:y,rowKey:"id",pagination:{showSizeChanger:!1,total:D,pageSize:x.pageSize,current:x.pageNo,onChange:function(p,a){var c={pageNo:p,pageSize:a};ie(c),g((0,s.Z)((0,s.Z)({},c),C))}},search:{},onSubmit:function(p){Z(p),g((0,s.Z)({pageNo:1,pageSize:10},p))},onReset:(0,F.Z)(u().mark(function h(){return u().wrap(function(a){for(;;)switch(a.prev=a.next){case 0:return a.next=2,Z({});case 2:return a.next=4,g({pageSize:10,pageNo:1});case 4:case"end":return a.stop()}},h)})),dateFormatter:"string",headerTitle:"\u5DF2\u6709\u6570\u636E\u5E93\u8868",toolBarRender:!1}),(0,r.jsx)(_,{ref:$,visible:re,setVisible:P,refresh:function(){g((0,s.Z)((0,s.Z)({},x),C))}})]})},ne=ae}}]);