(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[835],{20294:function(re){re.exports={rule:"rule___2uB8V",formContent:"formContent___mfpUm",cardContent:"cardContent___ADntJ",ruleDescDetail:"ruleDescDetail___2wYEk",highRisk:"highRisk___3vyHc",enableCol:"enableCol___1VWz8"}},58541:function(re,K,t){"use strict";t.r(K),t.d(K,{default:function(){return Fe}});var g=t(13062),F=t(71230),me=t(89032),i=t(15746),C=t(77576),z=t(12028),x=t(11849),he=t(57663),P=t(71577),ve=t(71153),k=t(60331),E=t(2824),d=t(67294),y=t(59975),J=t(74379),ae=t(38648),w=t(3182),ne=t(32059),G=t(94043),Q=t.n(G),M=t(37476),S=t(64317),Z=t(89460),W=t(13312),U=t(24480),e=t(85893),b="user_visited_time",j="user_visited_table",X="rule_table_count";function se(ie,le){var _=(0,d.useRef)(),oe=(0,d.useState)(!1),$=(0,E.Z)(oe,2),de=$[0],V=$[1],ce=(0,d.useState)({}),q=(0,E.Z)(ce,2),B=q[0],H=q[1],f=(0,d.useState)(),ee=(0,E.Z)(f,2),fe=ee[0],ue=ee[1],pe=(0,d.useState)(""),te=(0,E.Z)(pe,2),O=te[0],N=te[1];return(0,d.useImperativeHandle)(le,function(){return{setInitValues:function(R,A){H({});var D=R.split(",");(0,y.CZ)().then(function(c){ue(c.data),D.forEach(function(p){(0,y.hQ)({typeName:p}).then(function(m){var n,l,h=m.data,v=h.some(function(T){return parseFloat(T)<1}),I=[];v&&(I=h.map(function(T){return{id:U.Z.uuid(),label:"".concat(parseFloat(T)*100," %"),value:T}})),N(A),H(function(T){return(0,x.Z)((0,x.Z)({},T),{},(0,ne.Z)({},p,v?I:h))}),V(!0),(n=_.current)===null||n===void 0||n.setFieldsValue({ruleTableCount:c.data.ruleTableCount,ruleTablePeriod:c.data.ruleTablePeriod,ruleTimeRate:"".concat(parseFloat(c==null||(l=c.data)===null||l===void 0?void 0:l.ruleTimeRate)*100," %"),ruleTimePeriod:c.data.ruleTimePeriod})})})})}}}),(0,e.jsxs)(M.Y,{title:"\u4FEE\u6539\u89C4\u5219",layout:"horizontal",formRef:_,visible:de,onVisibleChange:V,width:350,labelAlign:"right",labelCol:{span:12},modalProps:{destroyOnClose:!1,onCancel:function(){var R;(R=_.current)===null||R===void 0||R.resetFields(),ie.setVisible(!1)}},onFinish:function(){var L=(0,w.Z)(Q().mark(function R(A){var D,c,p,m;return Q().wrap(function(l){for(;;)switch(l.prev=l.next){case 0:c=Object.assign(fe,A),typeof c.ruleTimeRate=="string"&&(D=c.ruleTimeRate)!==null&&D!==void 0&&D.includes("%")&&(m=(p=c.ruleTimeRate)===null||p===void 0?void 0:p.replace("%",""),c.ruleTimeRate=parseFloat(m)/100+""),delete c.id,(0,y._m)(c).then(function(h){var v=h.code;v===W.Gh&&(ae.Z.success({message:"\u63D0\u793A",description:"\u4FEE\u6539\u6210\u529F\uFF01"}),V(!1),ie.refresh())});case 4:case"end":return l.stop()}},R)}));return function(R){return L.apply(this,arguments)}}(),children:[O===j&&(0,e.jsxs)(e.Fragment,{children:[(0,e.jsx)(Z.A.Group,{children:(0,e.jsx)(S.Z,{rules:[{required:!0,message:"\u8BF7\u9009\u62E9\u8BBF\u95EE\u6B21\u6570\u6700\u5C0F\u9608\u503C\uFF01"}],width:"sm",name:"ruleTableCount",label:"\u8BBF\u95EE\u6B21\u6570\u6700\u5C0F\u9608\u503C",options:B==null?void 0:B.rule_table_count,tooltip:"\u4F4E\u4E8E\u6307\u5B9A\u6B21\u6570\u5C06\u88AB\u7B97\u4E3A\u5F02\u5E38",placeholder:"\u8BF7\u8F93\u5165\u8FB9\u754C\u6B21\u6570"})}),(0,e.jsx)(Z.A.Group,{children:(0,e.jsx)(S.Z,{rules:[{required:!0,message:"\u8BF7\u9009\u62E9\u8BBF\u7EDF\u8BA1\u5468\u671F\uFF01"}],width:"sm",name:"ruleTablePeriod",label:"\u7EDF\u8BA1\u5468\u671F\uFF08\u5929\uFF09",options:B==null?void 0:B.rule_table_period,tooltip:"\u8D85\u8FC7\u7EDF\u8BA1\u5468\u671F\u7684\u8BBF\u95EE\u884C\u4E3A\u5C06\u4E0D\u4F1A\u5F71\u54CD\u753B\u50CF",placeholder:"\u8BF7\u9009\u62E9\u7EDF\u8BA1\u5468\u671F"})})]}),O===b&&(0,e.jsxs)(e.Fragment,{children:[(0,e.jsx)(Z.A.Group,{children:(0,e.jsx)(S.Z,{rules:[{required:!0,message:"\u8BF7\u9009\u62E9\u8BBF\u95EE\u9891\u7387\u6700\u5C0F\u9608\u503C\uFF01"}],width:"sm",name:"ruleTimeRate",label:"\u8BBF\u95EE\u9891\u7387\u6700\u5C0F\u9608\u503C",options:B.rule_time_rate,tooltip:"\u4F4E\u4E8E\u9608\u503C\u5C06\u88AB\u89C6\u4E3A\u4E0D\u5E38\u8BBF\u95EE\u7684\u6570\u636E\u8868",placeholder:"\u8BF7\u8F93\u5165\u8FB9\u754C\u9891\u7387"})}),(0,e.jsx)(Z.A.Group,{children:(0,e.jsx)(S.Z,{rules:[{required:!0,message:"\u8BF7\u9009\u62E9\u8BBF\u7EDF\u8BA1\u5468\u671F\uFF01"}],width:"sm",name:"ruleTimePeriod",label:"\u7EDF\u8BA1\u5468\u671F\uFF08\u5929\uFF09",filedConfig:{},options:B.rule_time_period,tooltip:"\u8D85\u8FC7\u7EDF\u8BA1\u5468\u671F\u7684\u8BBF\u95EE\u884C\u4E3A\u5C06\u4E0D\u4F1A\u5F71\u54CD\u753B\u50CF",placeholder:"\u8BF7\u9009\u62E9\u7EDF\u8BA1\u5468\u671F"})})]})]})}var Y=(0,d.forwardRef)(se),r=t(29474),a=t(25377),o=t(20294),s=t.n(o),u=function(){var le=(0,d.useState)([]),_=(0,E.Z)(le,2),oe=_[0],$=_[1],de=(0,d.useState)(!1),V=(0,E.Z)(de,2),ce=V[0],q=V[1],B=(0,d.useState)(),H=(0,E.Z)(B,2),f=H[0],ee=H[1],fe=(0,d.useState)([]),ue=(0,E.Z)(fe,2),pe=ue[0],te=ue[1],O=(0,d.useRef)(),N=(0,a.md)(),L=(0,d.useState)({pageNo:1,pageSize:20}),R=(0,E.Z)(L,1),A=R[0],D=(0,d.useCallback)(function(p){(0,y.$D)(p).then(function(m){var n=m.data;if(n){var l=JSON.parse(n),h=JSON.parse(l.rows);h.length>0?$(h):$([])}}),(0,y.MY)().then(function(m){var n=m.data;Array.isArray(n)&&n.length>0&&(console.log(n),te([{title:"\u57FA\u4E8E\u6570\u636E\u5E93\u9AD8\u5371\u64CD\u4F5C\u7684\u5F02\u5E38\u544A\u8B66",id:U.Z.uuid(),enableRules:n.filter(function(l){return l.enable===1}),disableRules:n.filter(function(l){return l.enable===0})}]))})},[]),c=function(){(0,y.CZ)().then(function(m){var n=m.data;ee(n)})};return(0,d.useEffect)(function(){D(A),c()},[D,A]),(0,e.jsxs)("div",{className:s().rule,children:[(0,e.jsx)(r.ZP,{pagination:!1,rowKey:"id",rowSelection:!1,grid:{gutter:16,column:2},metas:{title:{dataIndex:"ruleDesc"},subTitle:{render:function(m,n){return[(0,e.jsx)(k.Z,{color:n.isDelete===1?"#f50":"#87d068",children:n.isDelete===1?"\u5DF2\u7981\u7528":"\u542F\u7528\u4E2D"},"tag")]}},type:{dataIndex:"gmtCreate"},content:{dataIndex:"ruleDesc",render:function(m,n){return(0,e.jsxs)("div",{className:s().cardContent,children:[(0,e.jsxs)("div",{className:s().ruleDescDetail,children:["\u521B\u5EFA\u65F6\u95F4\uFF1A",n.gmtCreate]},"ruleDesc"),n.ruleName===j&&(0,e.jsxs)("div",{className:s().ruleDescDetail,children:[(0,e.jsxs)("span",{children:["\u7EDF\u8BA1\u5468\u671F\uFF08\u5929\uFF09\uFF1A",f==null?void 0:f.ruleTablePeriod," \uFF08\u8D85\u8FC7\u7EDF\u8BA1\u5468\u671F\u7684\u8BBF\u95EE\u884C\u4E3A\u5C06\u4E0D\u4F1A\u5F71\u54CD\u753B\u50CF\uFF09"]}),(0,e.jsxs)("div",{children:["\u8BBF\u95EE\u6B21\u6570\u6700\u5C0F\u9608\u503C\uFF1A",f==null?void 0:f.ruleTableCount," \uFF08\u4F4E\u4E8E\u6307\u5B9A\u6B21\u6570\u5C06\u88AB\u7B97\u4E3A\u5F02\u5E38\uFF09"]})]},j),n.ruleName===b&&(0,e.jsxs)("div",{className:s().ruleDescDetail,children:[(0,e.jsxs)("span",{children:["\u7EDF\u8BA1\u5468\u671F\uFF08\u5929\uFF09\uFF1A",f==null?void 0:f.ruleTimePeriod," \uFF08\u8D85\u8FC7\u7EDF\u8BA1\u5468\u671F\u7684\u8BBF\u95EE\u884C\u4E3A\u5C06\u4E0D\u4F1A\u5F71\u54CD\u753B\u50CF\uFF09"]}),(0,e.jsxs)("div",{children:["\u8BBF\u95EE\u9891\u7387\u6700\u5C0F\u9608\u503C \uFF1A","".concat(parseFloat(f==null?void 0:f.ruleTimeRate)*100," %")," \uFF08\u4F4E\u4E8E\u9608\u503C\u5C06\u88AB\u89C6\u4E3A\u4E0D\u5E38\u8BBF\u95EE\u7684\u6570\u636E\u8868\uFF09"]})]},b),(0,e.jsx)("div",{className:s().ruleDescDetail,children:n.ruleDescDetail},"ruleDescDetail")]})}},actions:{render:function(m,n){return[(0,e.jsx)(a.Nv,{accessible:N.canUpdate,children:(0,e.jsx)(P.Z,{danger:n.isDelete===0,size:"small",type:"link",onClick:function(){(0,y.Y3)({ruleId:n.id,isDelete:n.isDelete===0?1:0}).then(function(){return D(A)})},children:n.isDelete===1?"\u542F\u7528":"\u7981\u7528"})},"isDelete"),(0,e.jsx)(a.Nv,{accessible:N.canUpdate,children:(0,e.jsx)(P.Z,{size:"small",type:"link",onClick:function(){if(j===n.ruleName){var h;(h=O.current)===null||h===void 0||h.setInitValues("rule_table_count,rule_table_period",j)}else if(b===n.ruleName){var v;(v=O.current)===null||v===void 0||v.setInitValues("rule_time_rate,rule_time_period",b)}},children:"\u7F16\u8F91"})},"update")]}}},headerTitle:"\u753B\u50CF\u89C4\u5219\u5217\u8868",dataSource:oe}),(0,e.jsx)(r.ZP,{pagination:!1,rowKey:"id",rowSelection:!1,grid:{gutter:16,column:2},metas:{title:{},type:{dataIndex:"gmtCreate"},content:{dataIndex:"ruleDesc",render:function(m,n){return(0,e.jsx)("div",{className:s().highRisk,children:(0,e.jsxs)(F.Z,{children:[(0,e.jsxs)(i.Z,{span:12,className:s().enableCol,children:[n.enableRules.length>0&&(0,e.jsx)("h5",{className:"primary-blue-5 m-0 p-0",children:"\u5DF2\u542F\u7528\u89C4\u5219"}),(0,e.jsxs)("div",{className:s().ruleDescDetail,children:[n.enableRules.map(function(l){return(0,e.jsxs)("div",{className:"flex-x-sb",children:[(0,e.jsxs)("span",{children:[l.keyword,"\uFF08",l.description,"\uFF09"]}),(0,e.jsx)(a.Nv,{accessible:N.canUpdate,children:(0,e.jsx)(z.Z,{size:"small",checkedChildren:"\u5F00\u542F",unCheckedChildren:"\u5173\u95ED",defaultChecked:!0,onChange:function(){var v=(0,x.Z)({},l);v.enable=0,(0,y.JH)([v]).then(function(I){var T=I.code;T===W.Gh&&D(A)})}})},"isSwitch")]},l.id)}),n.enableRules.length===0&&(0,e.jsx)("div",{children:"\u6682\u65E0..."})]})]}),(0,e.jsxs)(i.Z,{span:12,className:"pl-20",children:[n.enableRules.length>0&&(0,e.jsx)("h5",{className:"danger m-0 p-0 mt-5",children:"\u672A\u542F\u7528\u89C4\u5219"}),(0,e.jsxs)("div",{className:s().ruleDescDetail,children:[n.disableRules.map(function(l){return(0,e.jsxs)("div",{className:"flex-x-sb",children:[(0,e.jsxs)("span",{children:[l.keyword,"\uFF08",l.description,"\uFF09"]}),(0,e.jsx)(a.Nv,{accessible:N.canUpdate,children:(0,e.jsx)(z.Z,{size:"small",checkedChildren:"\u5F00\u542F",unCheckedChildren:"\u5173\u95ED",onChange:function(){var v=(0,x.Z)({},l);v.enable=1,(0,y.JH)([v]).then(function(I){var T=I.code;T===W.Gh&&D(A)})}})},"isSwitch1")]},l.id)}),n.disableRules.length===0&&(0,e.jsx)("div",{children:"\u6682\u65E0..."})]})]})]})})}}},headerTitle:"\u9AD8\u5371\u64CD\u4F5C\u89C4\u5219",dataSource:pe}),(0,e.jsx)(Y,{ref:O,refresh:function(){return c()},setVisible:q,visible:ce})]})},Fe=u},59975:function(re,K,t){"use strict";t.d(K,{f2:function(){return he},MN:function(){return E},yb:function(){return y},ny:function(){return ae},$D:function(){return ne},Y3:function(){return Q},hQ:function(){return S},CZ:function(){return W},_m:function(){return e},MY:function(){return j},JH:function(){return se}});var g=t(11849),F=t(3182),me=t(94043),i=t.n(me),C=t(25377),z=t(80129),x=t.n(z);function he(r,a){return P.apply(this,arguments)}function P(){return P=(0,F.Z)(i().mark(function r(a,o){return i().wrap(function(u){for(;;)switch(u.prev=u.next){case 0:return u.abrupt("return",(0,C.WY)("/api/skyflying/addUserPortraitByVisitedTableEverydayRule",(0,g.Z)({method:"GET",params:a},o||{})));case 1:case"end":return u.stop()}},r)})),P.apply(this,arguments)}function ve(r){return k.apply(this,arguments)}function k(){return k=_asyncToGenerator(_regeneratorRuntime.mark(function r(a){return _regeneratorRuntime.wrap(function(s){for(;;)switch(s.prev=s.next){case 0:return s.abrupt("return",request("/api/skyflying/updateUserPortraitByVisitedTimeRule",_objectSpread({method:"GET"},a||{})));case 1:case"end":return s.stop()}},r)})),k.apply(this,arguments)}function E(r,a){return d.apply(this,arguments)}function d(){return d=(0,F.Z)(i().mark(function r(a,o){return i().wrap(function(u){for(;;)switch(u.prev=u.next){case 0:return u.abrupt("return",(0,C.WY)("/api/skyflying/addUserPortraitByVisitedTtimeRule",(0,g.Z)({method:"GET",params:a},o||{})));case 1:case"end":return u.stop()}},r)})),d.apply(this,arguments)}function y(r,a){return J.apply(this,arguments)}function J(){return J=(0,F.Z)(i().mark(function r(a,o){return i().wrap(function(u){for(;;)switch(u.prev=u.next){case 0:return u.abrupt("return",(0,C.WY)("/api/skyflying/updateUserPortraitByVisitedTimeRule",(0,g.Z)({method:"GET",params:a},o||{})));case 1:case"end":return u.stop()}},r)})),J.apply(this,arguments)}function ae(r,a){return w.apply(this,arguments)}function w(){return w=(0,F.Z)(i().mark(function r(a,o){return i().wrap(function(u){for(;;)switch(u.prev=u.next){case 0:return u.abrupt("return",(0,C.WY)("/api/skyflying/updateUserPortraitByVisitedTableEverydayRule",(0,g.Z)({method:"GET",params:a},o||{})));case 1:case"end":return u.stop()}},r)})),w.apply(this,arguments)}function ne(r,a){return G.apply(this,arguments)}function G(){return G=(0,F.Z)(i().mark(function r(a,o){return i().wrap(function(u){for(;;)switch(u.prev=u.next){case 0:return u.abrupt("return",(0,C.WY)("/api/skyflying/getUserPortraitRules",(0,g.Z)({method:"GET",params:a},o||{})));case 1:case"end":return u.stop()}},r)})),G.apply(this,arguments)}function Q(r,a){return M.apply(this,arguments)}function M(){return M=(0,F.Z)(i().mark(function r(a,o){return i().wrap(function(u){for(;;)switch(u.prev=u.next){case 0:return u.abrupt("return",(0,C.WY)("/api/skyflying/updateUserPortraitRule?"+x().stringify(a),(0,g.Z)({method:"POST"},o||{})));case 1:case"end":return u.stop()}},r)})),M.apply(this,arguments)}function S(r,a){return Z.apply(this,arguments)}function Z(){return Z=(0,F.Z)(i().mark(function r(a,o){return i().wrap(function(u){for(;;)switch(u.prev=u.next){case 0:return u.abrupt("return",(0,C.WY)("/api/skyflying/config/getConfigDic",(0,g.Z)({method:"GET",params:a||{}},o||{})));case 1:case"end":return u.stop()}},r)})),Z.apply(this,arguments)}function W(r){return U.apply(this,arguments)}function U(){return U=(0,F.Z)(i().mark(function r(a){return i().wrap(function(s){for(;;)switch(s.prev=s.next){case 0:return s.abrupt("return",(0,C.WY)("/api/skyflying/config/getPortraitConfig",(0,g.Z)({method:"GET"},a||{})));case 1:case"end":return s.stop()}},r)})),U.apply(this,arguments)}function e(r,a){return b.apply(this,arguments)}function b(){return b=(0,F.Z)(i().mark(function r(a,o){return i().wrap(function(u){for(;;)switch(u.prev=u.next){case 0:return u.abrupt("return",(0,C.WY)("/api/skyflying/config/updatePortraitConfig",(0,g.Z)({method:"PUT",data:a},o||{})));case 1:case"end":return u.stop()}},r)})),b.apply(this,arguments)}function j(r){return X.apply(this,arguments)}function X(){return X=(0,F.Z)(i().mark(function r(a){return i().wrap(function(s){for(;;)switch(s.prev=s.next){case 0:return s.abrupt("return",(0,C.WY)("/api/skyflying/config/getAllHighRiskOpt",(0,g.Z)({method:"GET"},a||{})));case 1:case"end":return s.stop()}},r)})),X.apply(this,arguments)}function se(r,a){return Y.apply(this,arguments)}function Y(){return Y=(0,F.Z)(i().mark(function r(a,o){return i().wrap(function(u){for(;;)switch(u.prev=u.next){case 0:return u.abrupt("return",(0,C.WY)("/api/skyflying/config/updateHighRiskOpt",(0,g.Z)({method:"PUT",data:a},o||{})));case 1:case"end":return u.stop()}},r)})),Y.apply(this,arguments)}}}]);