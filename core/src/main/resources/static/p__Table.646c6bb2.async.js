(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[310],{97796:function(x){x.exports={table:"table___3lKck"}},45182:function(x,b,t){"use strict";t.r(b),t.d(b,{default:function(){return Q}});var q=t(57663),C=t(71577),_=t(62350),F=t(75443),tt=t(74379),P=t(38648),a=t(11849),p=t(2824),c=t(67294),I=t(97796),z=t.n(I),N=t(3182),B=t(94043),h=t.n(B),j=t(21704),O=t(80129),R=t.n(O);function G(o,s){return S.apply(this,arguments)}function S(){return S=(0,N.Z)(h().mark(function o(s,i){return h().wrap(function(e){for(;;)switch(e.prev=e.next){case 0:return e.abrupt("return",(0,j.WY)("/api/skyflying/getAllMonitorTables",(0,a.Z)({method:"GET",params:s},i||{})));case 1:case"end":return e.stop()}},o)})),S.apply(this,arguments)}function J(o,s){return T.apply(this,arguments)}function T(){return T=(0,N.Z)(h().mark(function o(s,i){return h().wrap(function(e){for(;;)switch(e.prev=e.next){case 0:return e.abrupt("return",(0,j.WY)("/api/skyflying/updateMonitorTable?"+R().stringify(s),(0,a.Z)({method:"POST"},i||{})));case 1:case"end":return e.stop()}},o)})),T.apply(this,arguments)}var K=t(47369),W=t(13312),Y=t(49101),l=t(85893),$=function(){var s=(0,c.useState)([]),i=(0,p.Z)(s,2),Z=i[0],e=i[1],V=(0,c.useState)(0),A=(0,p.Z)(V,2),H=A[0],E=A[1],L=(0,c.useState)({}),M=(0,p.Z)(L,2),y=M[0],U=M[1],X=(0,c.useState)({pageNo:1,pageSize:20}),D=(0,p.Z)(X,2),u=D[0],k=D[1],f=function(r){G(r).then(function(n){var v=n.data,d=JSON.parse(v);if(d&&d.rows){var m=JSON.parse(d.rows);if(m&&m.length>0){E(d.total),e(m);return}}E(0),e([])})};(0,c.useEffect)(function(){f((0,a.Z)({},u))},[]);var w=[{title:"\u6570\u636E\u5E93\u5730\u5740",dataIndex:"dbAddress",align:"center"},{title:"\u6570\u636E\u5E93\u540D",dataIndex:"dbName",align:"center",ellipsis:!0},{title:"\u8868\u540D",dataIndex:"tableName",align:"center"},{title:"\u72B6\u6001",align:"center",dataIndex:"isDelete",search:!1,initialValue:"0",valueEnum:{0:{text:"\u542F\u7528\u4E2D",status:"Success"},1:{text:"\u5DF2\u7981\u7528",status:"Error"}}},{title:"\u66F4\u65B0\u65F6\u95F4",key:"gmtModified",dataIndex:"gmtModified",align:"center",valueType:"dateTime",search:!1},{title:"\u64CD\u4F5C",key:"option",align:"center",valueType:"option",render:function(r,n){return[(0,l.jsx)(F.Z,{title:"\u60A8\u786E\u8BA4".concat(n.isDelete===0?"\u7981\u7528":"\u542F\u7528").concat(n.tableName,"\u8868\u5417\uFF1F"),placement:"top",onConfirm:function(){J({id:n.id,isDelete:n.isDelete===1?0:1}).then(function(d){var m=d.code;m===W.Gh&&(P.Z.success({message:"\u66F4\u65B0\u6210\u529F\uFF01"}),f((0,a.Z)((0,a.Z)({},u),y)))})},children:(0,l.jsx)("a",{className:n.isDelete===0?"danger":"",children:n.isDelete===0?"\u7981\u7528":"\u542F\u7528"})},"link")]}}];return(0,l.jsx)("div",{className:z().table,children:(0,l.jsx)(K.ZP,{columns:w,dataSource:Z,rowKey:"id",pagination:{showQuickJumper:!0,total:H,pageSize:u.pageSize,current:u.pageNo,onChange:function(r,n){var v={pageNo:r,pageSize:n};k(v),f((0,a.Z)((0,a.Z)({},v),y))}},search:{},onSubmit:function(r){U(r),f((0,a.Z)((0,a.Z)({},u),r))},onReset:function(){f((0,a.Z)((0,a.Z)({},u),y))},dateFormatter:"string",headerTitle:"\u5DF2\u6709\u6570\u636E\u5E93\u8868",toolBarRender:function(){return[(0,l.jsx)(C.Z,{icon:(0,l.jsx)(Y.Z,{}),type:"primary",children:"\u65B0\u5EFA"},"button")]}})})},Q=$}}]);
