(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[310],{5966:function(O,M,e){"use strict";var V=e(22122),j=e(81253),B=e(67294),A=e(30885),a=["fieldProps","proFieldProps"],d=["fieldProps","proFieldProps"],n="text",U=function(c){var y=c.fieldProps,f=c.proFieldProps,x=(0,j.Z)(c,a);return B.createElement(A.Z,(0,V.Z)({mode:"edit",valueType:n,fieldProps:y,filedConfig:{valueType:n},proFieldProps:f},x))},W=function(c){var y=c.fieldProps,f=c.proFieldProps,x=(0,j.Z)(c,d);return B.createElement(A.Z,(0,V.Z)({mode:"edit",valueType:"password",fieldProps:y,proFieldProps:f,filedConfig:{valueType:n}},x))},R=U;R.Password=W,R.displayName="ProFormComponent",M.Z=R},97796:function(O){O.exports={table:"table___3lKck"}},24927:function(O,M,e){"use strict";e.r(M),e.d(M,{default:function(){return ue}});var V=e(62350),j=e(75443),B=e(74379),A=e(38648),a=e(11849),d=e(2824),n=e(67294),U=e(97796),W=e.n(U),R=e(71194),z=e(50146),c=e(57663),y=e(71577),f=e(3182),x=e(94043),p=e.n(x),H=e(5894),q=e(5966),K=e(21704),_=e(80129),ee=e.n(_);function te(r,l){return $.apply(this,arguments)}function $(){return $=(0,f.Z)(p().mark(function r(l,o){return p().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,K.WY)("/api/skyflying/getAllMonitorTables",(0,a.Z)({method:"GET",params:l},o||{})));case 1:case"end":return t.stop()}},r)})),$.apply(this,arguments)}function ae(r,l){return G.apply(this,arguments)}function G(){return G=(0,f.Z)(p().mark(function r(l,o){return p().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,K.WY)("/api/skyflying/updateMonitorTable?"+ee().stringify(l),(0,a.Z)({method:"POST"},o||{})));case 1:case"end":return t.stop()}},r)})),G.apply(this,arguments)}function ne(r,l){return L.apply(this,arguments)}function L(){return L=(0,f.Z)(p().mark(function r(l,o){return p().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,K.WY)("/api/skyflying/updateMonitorTableDesc",(0,a.Z)({method:"POST",data:l},o||{})));case 1:case"end":return t.stop()}},r)})),L.apply(this,arguments)}var s=e(85893);function re(r,l){var o={labelCol:{span:6},wrapperCol:{span:20}},T=(0,n.useState)(),t=(0,d.Z)(T,2),F=t[0],I=t[1],N=(0,n.useRef)();return(0,n.useImperativeHandle)(l,function(){return{setInitValues:function(m){var v;I(m),(v=N.current)===null||v===void 0||v.setFieldsValue({tableDesc:m.tableDesc})}}}),(0,s.jsx)(z.Z,{width:400,title:"\u4FEE\u6539\u63CF\u8FF0",visible:r.visible,footer:[],onCancel:function(){return r.setVisible(!1)},children:(0,s.jsxs)(H.A,(0,a.Z)((0,a.Z)({formRef:N,layout:"horizontal"},o),{},{onFinish:function(){var b=(0,f.Z)(p().mark(function m(v){return p().wrap(function(E){for(;;)switch(E.prev=E.next){case 0:ne({id:F==null?void 0:F.id,tableDesc:v.tableDesc}).then(function(){r.refresh(),r.setVisible(!1)});case 1:case"end":return E.stop()}},m)}));return function(m){return b.apply(this,arguments)}}(),submitter:!1,children:[(0,s.jsx)(q.Z,{fieldProps:{placeholder:"\u8BF7\u8F93\u5165\u8868\u7684\u63CF\u8FF0\u6587\u5B57"},label:"\u63CF\u8FF0\u6587\u5B57",rules:[{required:!0,message:"\u8BF7\u586B\u5199\u63CF\u8FF0\u6587\u5B57"}],name:"tableDesc"}),(0,s.jsx)(H.A.Group,{style:{textAlign:"center"},children:(0,s.jsx)(y.Z,{type:"primary",htmlType:"submit",className:"w-100",children:"\u786E\u8BA4\u4FEE\u6539"})})]}))})}var se=(0,n.forwardRef)(re),Y=e(32773),le=e(47369),ie=e(13312),oe=function(){var l=(0,n.useState)([]),o=(0,d.Z)(l,2),T=o[0],t=o[1],F=(0,n.useState)(0),I=(0,d.Z)(F,2),N=I[0],b=I[1],m=(0,n.useState)({}),v=(0,d.Z)(m,2),S=v[0],E=v[1],de=(0,n.useState)(!1),Q=(0,d.Z)(de,2),ce=Q[0],X=Q[1],w=(0,n.useRef)(),J=(0,Y.md)(),fe=(0,n.useState)({pageNo:1,pageSize:10}),k=(0,d.Z)(fe,2),h=k[0],pe=k[1];console.log(J.canUpdate());var P=function(g){te(g).then(function(i){var Z=i.data,u=JSON.parse(Z);if(u&&u.rows){var C=JSON.parse(u.rows);if(C&&C.length>0){b(u.total),t(C);return}}b(0),t([])})};(0,n.useEffect)(function(){P((0,a.Z)({},h))},[]);var ve=[{title:"\u6570\u636E\u5E93\u5730\u5740",dataIndex:"dbAddress",align:"center"},{title:"\u6570\u636E\u5E93\u540D",dataIndex:"dbName",align:"center",ellipsis:!0},{title:"\u8868\u540D",dataIndex:"tableName",align:"center"},{title:"\u63CF\u8FF0",dataIndex:"tableDesc",align:"center"},{title:"\u72B6\u6001",align:"center",dataIndex:"isDelete",search:!1,initialValue:"0",valueEnum:{0:{text:"\u542F\u7528\u4E2D",status:"Success"},1:{text:"\u5DF2\u7981\u7528",status:"Error"}}},{title:"\u66F4\u65B0\u65F6\u95F4",key:"gmtModified",dataIndex:"gmtModified",align:"center",valueType:"dateTime",search:!1},{title:"\u64CD\u4F5C",key:"option",align:"center",valueType:"option",render:function(g,i){return[(0,s.jsx)(Y.Nv,{accessible:J.canUpdate(),children:(0,s.jsx)(j.Z,{title:"\u60A8\u786E\u8BA4".concat(i.isDelete===0?"\u7981\u7528":"\u542F\u7528").concat(i.tableName,"\u8868\u5417\uFF1F"),placement:"top",onConfirm:function(){ae({id:i.id,isDelete:i.isDelete===1?0:1}).then(function(u){var C=u.code;C===ie.Gh&&(A.Z.success({message:"\u66F4\u65B0\u6210\u529F\uFF01"}),P((0,a.Z)((0,a.Z)({},h),S)))})},children:(0,s.jsx)("a",{className:i.isDelete===0?"danger":"",children:i.isDelete===0?"\u7981\u7528":"\u542F\u7528"})})},"link"),(0,s.jsx)(Y.Nv,{accessible:J.canUpdate(),children:(0,s.jsx)("a",{onClick:function(){var u;(u=w.current)===null||u===void 0||u.setInitValues(i),X(!0)},children:"\u66F4\u65B0"})},"update")]}}];return(0,s.jsxs)("div",{className:W().table,children:[(0,s.jsx)(le.ZP,{columns:ve,dataSource:T,rowKey:"id",pagination:{showSizeChanger:!1,total:N,pageSize:h.pageSize,current:h.pageNo,onChange:function(g,i){var Z={pageNo:g,pageSize:i};pe(Z),P((0,a.Z)((0,a.Z)({},Z),S))}},search:{},onSubmit:function(g){E(g),P((0,a.Z)((0,a.Z)({},h),g))},onReset:function(){P((0,a.Z)((0,a.Z)({},h),S))},dateFormatter:"string",headerTitle:"\u5DF2\u6709\u6570\u636E\u5E93\u8868",toolBarRender:!1}),(0,s.jsx)(se,{ref:w,visible:ce,setVisible:X,refresh:function(){P((0,a.Z)((0,a.Z)({},h),S))}})]})},ue=oe}}]);
