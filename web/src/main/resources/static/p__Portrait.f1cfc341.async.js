(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[990],{49027:function(I){I.exports={portrait:"portrait___2weew","portrait__form-wrap":"portrait__form-wrap___2wsdq",table:"table___95yq2"}},51182:function(I,O,e){"use strict";e.r(O),e.d(O,{default:function(){return Ge}});var _e=e(66456),pe=e(78587),et=e(13062),ce=e(71230),tt=e(89032),$=e(15746),g=e(3182),rt=e(48736),ve=e(27049),at=e(57663),y=e(71577),it=e(74379),J=e(38648),Z=e(11849),m=e(2824),fe=e(94043),p=e.n(fe),a=e(67294),me=e(49027),A=e.n(me),K=e(76570),he=e(25592),F=e(22122),C=e(28991),T=e(81253),S=e(30885),j=e(66758),Pe=["proFieldProps","fieldProps"],H="date",ge=a.forwardRef(function(r,s){var o=r.proFieldProps,i=r.fieldProps,l=(0,T.Z)(r,Pe),v=(0,a.useContext)(j.Z);return a.createElement(S.Z,(0,F.Z)({ref:s,mode:"edit",valueType:H,fieldProps:(0,C.Z)({getPopupContainer:v.getPopupContainer},i),proFieldProps:o,filedConfig:{valueType:H,customLightMode:!0}},l))}),ye=ge,Ze=["proFieldProps","fieldProps"],X="dateWeek",xe=a.forwardRef(function(r,s){var o=r.proFieldProps,i=r.fieldProps,l=(0,T.Z)(r,Ze),v=(0,a.useContext)(j.Z);return a.createElement(S.Z,(0,F.Z)({ref:s,mode:"edit",valueType:X,fieldProps:(0,C.Z)({getPopupContainer:v.getPopupContainer},i),proFieldProps:o,filedConfig:{valueType:X,customLightMode:!0}},l))}),Fe=xe,Ce=["proFieldProps","fieldProps"],q="dateMonth",Te=a.forwardRef(function(r,s){var o=r.proFieldProps,i=r.fieldProps,l=(0,T.Z)(r,Ce),v=(0,a.useContext)(j.Z);return a.createElement(S.Z,(0,F.Z)({ref:s,mode:"edit",valueType:q,fieldProps:(0,C.Z)({getPopupContainer:v.getPopupContainer},i),proFieldProps:o,filedConfig:{valueType:q,customLightMode:!0}},l))}),Se=Te,je=["fieldProps"],_="dateQuarter",Ee=a.forwardRef(function(r,s){var o=r.fieldProps,i=(0,T.Z)(r,je),l=(0,a.useContext)(j.Z);return a.createElement(S.Z,(0,F.Z)({ref:s,mode:"edit",valueType:_,fieldProps:(0,C.Z)({getPopupContainer:l.getPopupContainer},o),filedConfig:{valueType:_,customLightMode:!0}},i))}),be=Ee,Be=["proFieldProps","fieldProps"],ee="dateYear",De=a.forwardRef(function(r,s){var o=r.proFieldProps,i=r.fieldProps,l=(0,T.Z)(r,Be),v=(0,a.useContext)(j.Z);return a.createElement(S.Z,(0,F.Z)({ref:s,mode:"edit",valueType:ee,fieldProps:(0,C.Z)({getPopupContainer:v.getPopupContainer},i),proFieldProps:o,filedConfig:{valueType:ee,customLightMode:!0}},l))}),Ne=De,x=ye;x.Week=Fe,x.Month=Se,x.Quarter=be,x.Year=Ne,x.displayName="ProFormComponent";var te=x,b=e(5894),k=e(64317),M=e(25377);function Ve(r){return w.apply(this,arguments)}function w(){return w=(0,g.Z)(p().mark(function r(s){return p().wrap(function(i){for(;;)switch(i.prev=i.next){case 0:return i.abrupt("return",(0,M.WY)("/api/skyflying/getAllUserNameUserPortraitByVisitedTableEveryday",(0,Z.Z)({method:"GET"},s||{})));case 1:case"end":return i.stop()}},r)})),w.apply(this,arguments)}function Re(r){return W.apply(this,arguments)}function W(){return W=(0,g.Z)(p().mark(function r(s){return p().wrap(function(i){for(;;)switch(i.prev=i.next){case 0:return i.abrupt("return",(0,M.WY)("/api/skyflying/getAllVisitedTablePortraitByVisitedTableEveryday",(0,Z.Z)({method:"GET"},s||{})));case 1:case"end":return i.stop()}},r)})),W.apply(this,arguments)}function ze(r,s){return Q.apply(this,arguments)}function Q(){return Q=(0,g.Z)(p().mark(function r(s,o){return p().wrap(function(l){for(;;)switch(l.prev=l.next){case 0:return l.abrupt("return",(0,M.WY)("/api/skyflying/getUserPortraitByVisitedTableEveryday",(0,Z.Z)({method:"GET",params:s||{}},o||{})));case 1:case"end":return l.stop()}},r)})),Q.apply(this,arguments)}var re=e(59975),st=e(71194),Ae=e(50146),nt=e(20228),ke=e(11382),ae=e(952),ie=e(5966),Me=e(31199),we=["select","insert","update","delete"],lt=[{level:"\u9AD8\u5371",color:"#D9534F",value:1},{level:"\u4E2D\u5371",color:"#F0AD4E",value:2},{level:"\u4F4E\u5371",color:"#FFF600",value:3}],t=e(85893),We=function(s){var o=(0,a.useRef)(),i=(0,a.useState)(!1),l=(0,m.Z)(i,2),v=l[0],B=l[1],D={labelCol:{span:8},wrapperCol:{span:16}};return(0,t.jsx)(Ae.Z,{width:700,title:"\u6DFB\u52A0\u7528\u6237\u5BF9\u8868\u8BBF\u95EE\u7684\u753B\u50CF\u89C4\u5219",visible:s.tableRuleVisible,footer:[],onCancel:function(){return s.setVisible(!1)},children:(0,t.jsx)(ke.Z,{spinning:v,children:(0,t.jsxs)(ae.ZP,(0,Z.Z)((0,Z.Z)({size:"middle",layout:"horizontal"},D),{},{submitter:!1,onFinish:function(){var N=(0,g.Z)(p().mark(function V(Y){return p().wrap(function(E){for(;;)switch(E.prev=E.next){case 0:B(!0),(0,re.f2)(Y).then(function(R){var L=R.code;L==="0000"&&(J.Z.success({message:"\u521B\u5EFA\u6210\u529F!",placement:"top"}),B(!1),s.setVisible(!1))});case 2:case"end":return E.stop()}},V)}));return function(V){return N.apply(this,arguments)}}(),formRef:o,children:[(0,t.jsx)(ie.Z,{name:"userName",width:"md",placeholder:"\u8BF7\u8F93\u5165\u7528\u6237\u540D",label:"\u767B\u5F55\u7CFB\u7EDF\u7528\u6237\u540D",rules:[{required:!0,message:"\u8BF7\u8F93\u5165\u767B\u5F55\u7CFB\u7EDF\u7528\u6237\u540D"}],required:!0}),(0,t.jsx)(ie.Z,{name:"visitedTable",width:"md",label:"\u7528\u6237\u8BBF\u95EE\u8868",placeholder:"\u8BF7\u8F93\u5165\u8868\u540D",rules:[{required:!0,message:"\u8BF7\u8F93\u5165\u7528\u6237\u8BBF\u95EE\u8868"}],required:!0}),(0,t.jsx)(Me.Z,{label:"\u8BBF\u95EE\u8868\u6B21\u6570",placeholder:"\u8BF7\u8F93\u5165\u6B21\u6570",name:"visitedCount",initialValue:1,width:"sm",min:1,rules:[{required:!0,message:"\u8BF7\u8F93\u5165\u8BBF\u95EE\u8868\u6B21\u6570"}],required:!0}),(0,t.jsx)(te,{label:"\u8BBF\u95EE\u8FC7\u8868\u7684\u65F6\u95F4",name:"visitedDate",placeholder:"\u8BF7\u9009\u62E9\u8BBF\u95EE\u65F6\u95F4",width:"sm",rules:[{required:!0,message:"\u8BF7\u9009\u62E9\u8BBF\u95EE\u65F6\u95F4\uFF01"}],required:!0}),(0,t.jsx)(k.Z,{label:"SQL\u7C7B\u578B",name:"dbType",options:we,width:"sm",initialValue:"select",placeholder:"\u8BF7\u9009\u62E9SQL\u7C7B\u578B",rules:[{required:!0,message:"\u8BF7\u9009\u62E9SQL\u7C7B\u578B\uFF01"}],required:!0}),(0,t.jsxs)(ae.ZP.Group,{style:{textAlign:"center",marginTop:"30px"},children:[(0,t.jsx)(y.Z,{type:"primary",htmlType:"submit",className:"w-100",children:"\u521B\u5EFA\u89C4\u5219"}),(0,t.jsx)(y.Z,{className:"w-100",htmlType:"reset",children:"\u91CD \u7F6E"})]})]}))})})},Qe=We,Ye=function(){var s=(0,a.useRef)(),o=(0,a.useState)([]),i=(0,m.Z)(o,2),l=i[0],v=i[1],B=(0,a.useState)([]),D=(0,m.Z)(B,2),N=D[0],V=D[1],Y=(0,a.useState)([]),G=(0,m.Z)(Y,2),E=G[0],R=G[1],L=(0,a.useState)(!1),se=(0,m.Z)(L,2),Le=se[0],ne=se[1],Ue=(0,a.useState)(0),le=(0,m.Z)(Ue,2),Ie=le[0],Oe=le[1],$e=(0,a.useState)({pageNo:1,pageSize:10}),ue=(0,m.Z)($e,2),h=ue[0],Je=ue[1],Ke=(0,a.useState)((0,Z.Z)({},h)),oe=(0,m.Z)(Ke,2),U=oe[0],He=oe[1],P=(0,a.useCallback)(function(d){var n=d||h;ze(n).then(function(u){var f=u.data;if(f){var c=JSON.parse(f);if(Oe(c.total),c){var de=JSON.parse(c.rows);R(de)}else R([])}})},[h]),Xe=[{title:"\u7528\u6237\u540D",dataIndex:"userName",key:"userName",align:"center"},{title:"\u8BBF\u95EE\u65E5\u671F\uFF08\u6309\u7167\u5929\u8BA1\u7B97\uFF09",dataIndex:"visitedDate",key:"visitedDate",align:"center"},{title:"\u8BBF\u95EE\u6B21\u6570",dataIndex:"visitedCount",key:"visitedCount",align:"center"},{title:"\u8BBF\u95EE\u8868",dataIndex:"visitedTable",key:"visitedTable",align:"center"},{title:"\u64CD\u4F5C\u7C7B\u578B",dataIndex:"dbType",key:"dbType",align:"center"},{title:"\u64CD\u4F5C",key:"action",width:120,align:"center",render:function(n){return(0,t.jsx)(y.Z,{size:"small",type:"link",danger:n.isDelete===0,onClick:function(){(0,re.ny)({ruleId:n.id,isDelete:n.isDelete===0?1:0}).then(function(f){var c=f.code;c==="0000"&&(J.Z.success({message:"\u64CD\u4F5C\u6210\u529F!",placement:"top"}),P())})},children:n.isDelete===0?"\u7981\u7528":"\u542F\u7528"})}}],qe=function(n){n&&Object.keys(n).length>0&&He(function(u){return{pageSize:u.pageSize,pageNo:u.pageNo,userName:n.userName,visitedDate:n.visitedDate,visitedTable:n.visitedTable}}),P(n)};return(0,a.useEffect)(function(){Ve().then(function(d){var n=d.data,u=JSON.parse(n);v(u)}),Re().then(function(d){var n=d.data,u=JSON.parse(n);V(u)}),P()},[P]),(0,t.jsxs)("div",{className:A().portrait,children:[(0,t.jsxs)("div",{className:A()["portrait__form-wrap"],children:[(0,t.jsx)(ve.Z,{orientation:"left",plain:!0,children:(0,t.jsx)(K.Z,{})}),(0,t.jsxs)(ce.Z,{children:[(0,t.jsx)($.Z,{span:20,children:(0,t.jsxs)(b.A,{layout:"inline",formRef:s,size:"small",onFinish:function(){var d=(0,g.Z)(p().mark(function n(u){return p().wrap(function(c){for(;;)switch(c.prev=c.next){case 0:qe(u);case 1:case"end":return c.stop()}},n)}));return function(n){return d.apply(this,arguments)}}(),submitter:{render:function(){return[(0,t.jsx)(y.Z,{type:"primary",htmlType:"submit",icon:(0,t.jsx)(K.Z,{}),className:"br-5 w-80 fs-12",children:"\u67E5\u8BE2"},"edit"),(0,t.jsx)(y.Z,{htmlType:"button",icon:(0,t.jsx)(he.Z,{}),onClick:function(){var u;(u=s.current)===null||u===void 0||u.resetFields(),P({pageNo:h.pageNo,pageSize:h.pageSize})},className:"br-5 w-80 fs-12",children:"\u91CD\u7F6E"},"reset")]}},children:[(0,t.jsx)(b.A.Group,{children:(0,t.jsx)(k.Z,{options:l,placeholder:"\u7528\u6237\u540D",name:"userName",filedConfig:{},fieldProps:{}},"value")}),(0,t.jsx)(b.A.Group,{children:(0,t.jsx)(k.Z,{options:N,width:"sm",placeholder:"\u6570\u636E\u5E93\u8868\u540D",name:"visitedTable",filedConfig:{},fieldProps:{}},"value")}),(0,t.jsx)(b.A.Group,{children:(0,t.jsx)(te,{width:"sm",fieldProps:{bordered:!0,placeholder:"\u8BBF\u95EE\u65F6\u95F4"},name:"visitedDate"})})]})}),(0,t.jsx)($.Z,{span:4,children:(0,t.jsx)("div",{className:"text-r",children:(0,t.jsx)(y.Z,{className:"br-5 w-80 fs-12",size:"small",type:"primary",onClick:function(){ne(!0)},children:"\u65B0\u589E\u89C4\u5219"})})})]})]}),(0,t.jsx)("div",{className:A().table,children:(0,t.jsx)(pe.Z,{columns:Xe,dataSource:E,rowKey:"id",size:"middle",pagination:{showSizeChanger:!1,current:h.pageNo,pageSize:h.pageSize,total:Ie,onChange:function(){var d=(0,g.Z)(p().mark(function u(f,c){return p().wrap(function(z){for(;;)switch(z.prev=z.next){case 0:return z.next=2,Je(function(){return{pageSize:c,pageNo:f}});case 2:setTimeout(function(){P({pageNo:f,pageSize:c,userName:U.userName,visitedDate:U.visitedDate,visitedTable:U.visitedTable})},200);case 3:case"end":return z.stop()}},u)}));function n(u,f){return d.apply(this,arguments)}return n}()}})}),(0,t.jsx)(Qe,{tableRuleVisible:Le,setVisible:ne,refresh:P})]})},Ge=Ye}}]);
