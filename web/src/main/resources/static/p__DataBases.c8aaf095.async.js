(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[383],{12265:function(A){A.exports={databases:"databases___35H4X","detail-wrap":"detail-wrap___1IA0t","tops-wrap":"tops-wrap___2AQns",tops:"tops___2lHCZ"}},59498:function(A,C,e){"use strict";e.r(C),e.d(C,{default:function(){return le}});var d=e(3182),u=e(11849),N=e(57663),i=e(71577),g=e(34792),Z=e(48086),h=e(86582),m=e(2824),D=e(94043),E=e.n(D),r=e(67294),T=e(83539),R=e(84674),W=e(12265),B=e.n(W),F=e(24480),x=e(66572),$=e(13062),S=e(71230),ae=e(89032),O=e(15746),s=e(69886),a=e(85893),l=function(o){var b=(0,r.useCallback)(function(){var c=new s.by("dataActionPie".concat(o.id),{autoFit:!0,height:250,appendPadding:10,data:o.types,angleField:"value",colorField:"type",radius:.75,legend:{position:"bottom"},label:{type:"spider",labelHeight:28,content:`{name}
{percentage}`},interactions:[{type:"element-selected"},{type:"element-active"}]});c.update({theme:{styleSheet:{brandColor:"#025DF4",paletteQualitative10:["#025DF4","#DB6BCF","#2498D1","#BBBDE6","#4045B2","#21A97A","#FF745A","#007E99","#FFA8A8","#2391FF"],paletteQualitative20:["#025DF4","#DB6BCF","#2498D1","#BBBDE6","#4045B2","#21A97A","#FF745A","#007E99","#FFA8A8","#2391FF","#FFC328","#A0DC2C","#946DFF","#626681","#EB4185","#CD8150","#36BCCB","#327039","#803488","#83BC99"]}}}),c.render()},[o.id]);return(0,r.useEffect)(function(){b()},[b]),(0,a.jsx)("div",{id:"dataActionPie"+o.id})},_=l,t=e(39004),ne=function(o){var b=(0,r.useCallback)(function(){var c=new t.kL({container:"dataRecentlyLine".concat(o.id),autoFit:!0,height:200});c.data(o.ranges),c.scale({value:{min:0,nice:!1},year:{range:[0,1]}}),c.tooltip({showNil:!1,showCrosshairs:!1,title:function(Y,w){return"".concat(Y,"\uFF1A").concat(w.value," \u6B21")},itemTpl:`<div class="g2-tooltip">
  <div class="g2-tooltip-title">Language</div>
  <ul class="g2-tooltip-list">
    <li class="g2-tooltip-list-item">
      <span class="g2-tooltip-marker"></span>
      <span class="g2-tooltip-name">a</span>:<span class="g2-tooltip-value">70</span>
    </li>
    <li class="g2-tooltip-list-item">
      <span class="g2-tooltip-marker"></span>
      <span class="g2-tooltip-name">b</span>:<span class="g2-tooltip-value">50</span>
    </li>
  </ul>
</div>`}),c.axis("value",{label:{}}),c.area().position("year*value"),c.line().position("year*value"),c.render()},[o.id]);return(0,r.useEffect)(function(){b()},[b]),(0,a.jsx)("div",{id:"dataRecentlyLine"+o.id})},se=ne,re=function(o){return(0,a.jsx)(S.Z,{className:o.style["detail-wrap"],gutter:20,children:(0,a.jsx)(O.Z,{span:24,children:(0,a.jsxs)(S.Z,{children:[(0,a.jsxs)(O.Z,{span:12,className:"text-c",children:[(0,a.jsx)("h3",{children:"\u64CD\u4F5C\u884C\u4E3A\u5206\u5E03"}),(0,a.jsx)(_,{types:o.types,id:o.id})]}),(0,a.jsx)(O.Z,{span:12,className:"text-c",children:(0,a.jsxs)("div",{className:"text-c mt-15 mb-20",children:[(0,a.jsx)("h3",{children:"\u8FD1\u4E03\u5929\u8BBF\u95EE\u6570\u636E\u7EDF\u8BA1"}),(0,a.jsx)(se,{ranges:o.ranges,id:o.id})]})})]})})})},ue=re,K=e(6650),ie=function(){var o=(0,r.useState)([]),b=(0,m.Z)(o,2),c=b[0],L=b[1],Y=(0,r.useState)(""),w=(0,m.Z)(Y,2),V=w[0],z=w[1],oe=(0,r.useState)(0),H=(0,m.Z)(oe,2),de=H[0],ce=H[1],pe=(0,r.useState)(),J=(0,m.Z)(pe,2),me=J[0],fe=J[1],ve=(0,r.useState)({pageSize:10,pageNo:1}),Q=(0,m.Z)(ve,2),U=Q[0],ge=Q[1],he=(0,r.useState)({types:[],ranges:[]}),X=(0,m.Z)(he,2),_e=X[0],q=X[1],ye=[{title:"\u6570\u636E\u5E93\u8868",dataIndex:"tableName",key:"tableName",align:"center",render:function(f,n){var y="-";if(n.tableNameDesc)y=n.tableNameDesc;else{var p=n.tableName.split("#");y=p[p.length-1]}return(0,a.jsx)("div",{style:{wordWrap:"break-word",wordBreak:"break-word"},children:y})}},{title:"\u88AB\u8BBF\u95EE\u6B21\u6570",dataIndex:"visitedCount",key:"visitedCount",align:"center",search:!1},{title:"\u5E38\u8BBF\u95EE\u4EBA\u5458",key:"usualVisitedUser",dataIndex:"usualVisitedUser",align:"center",search:!1},{title:"\u6700\u540E\u8BBF\u95EE\u65F6\u95F4",key:"lastVisitedDate",dataIndex:"lastVisitedDate",valueType:"dateTime",hideInSetting:!0,search:!1,align:"center"},{title:"\u64CD\u4F5C",key:"action",align:"center",search:!1,render:function(f,n){return[(0,a.jsx)(i.Z,{type:"link",size:"small",onClick:function(){if(V===n.id){z(""),q({types:[],ranges:[]});return}if(n.visitedCount!==0){var p=F.Z.getRangeStartAndEndTime(7,"YYYY-mm-dd HH:MM:SS"),M=F.Z.getRangeTime(7,"YYYY-mm-dd");(0,K.Xd)({msTableName:n.tableName,startTime:p[0],endTime:p[1],tableName:n.tableName}).then(function(Ce){var G=Ce.data;if(G&&G.length>0){var De=G.map(function(j,I){return{year:M[I],value:j}}),ee=(0,h.Z)(R.q);(0,K.et)({tableName:n.tableName}).then(function(j){j.data&&j.data.length>0&&ee.forEach(function(I){j.data.forEach(function(Ee){var te=JSON.parse(Ee),be=te.dbType,Te=te.dbTypeTimes;I.type===be&&(I.value=parseInt(Te))})})}),q({ranges:De,types:ee}),setTimeout(function(){z(n.id)},400)}})}else Z.ZP.info("".concat(n.tableName,"\u8868\u8BBF\u88AB\u95EE\u6B21\u6570\u4E3A0\uFF0C\u6682\u65E0\u8BE6\u60C5\u6570\u636E")).then()},children:"\u8BE6\u60C5"},"action")]}}],P=(0,r.useCallback)(function(v){(0,x.m)(v).then(function(f){var n=f.data;if(n){var y=JSON.parse(n);if(ce(y.total),y.rows){var p=JSON.parse(y.rows);L(p.map(function(M){return M.id=F.Z.uuid(),M}));return}}L([])})},[]);return(0,r.useEffect)(function(){P(U)},[P,U]),(0,a.jsx)("div",{className:B().databases,children:(0,a.jsx)(T.ZP,{columns:ye,rowKey:"id",pagination:{showSizeChanger:!1,total:de,pageSize:U.pageSize,current:U.pageNo,onChange:function(f,n){ge({pageNo:f,pageSize:n}),P((0,u.Z)({pageNo:f,pageSize:n},me))}},toolBarRender:!1,onSubmit:function(){var v=(0,d.Z)(E().mark(function f(n){return E().wrap(function(p){for(;;)switch(p.prev=p.next){case 0:return fe(n),p.next=3,P((0,u.Z)((0,u.Z)({},U),n));case 3:case"end":return p.stop()}},f)}));return function(f){return v.apply(this,arguments)}}(),onReset:(0,d.Z)(E().mark(function v(){return E().wrap(function(n){for(;;)switch(n.prev=n.next){case 0:P((0,u.Z)({},U));case 1:case"end":return n.stop()}},v)})),expandable:{expandedRowRender:function(f,n){return(0,r.createElement)(ue,(0,u.Z)((0,u.Z)({},_e),{},{id:n,key:n,style:B()}))},expandIcon:function(){return!1},expandedRowKeys:[V]},dataSource:c})})},le=ie},84674:function(A,C,e){"use strict";e.d(C,{q:function(){return d},Q:function(){return u}});var d=[{type:"insert",value:0},{type:"delete",value:0},{type:"update",value:0},{type:"select",value:0}],u=[{type:"\u4E0A\u5348",value:0},{type:"\u4E0B\u5348",value:0},{type:"\u665A\u4E0A",value:0}]},66572:function(A,C,e){"use strict";e.d(C,{m:function(){return Z}});var d=e(11849),u=e(3182),N=e(94043),i=e.n(N),g=e(25377);function Z(m,D){return h.apply(this,arguments)}function h(){return h=(0,u.Z)(i().mark(function m(D,E){return i().wrap(function(T){for(;;)switch(T.prev=T.next){case 0:return T.abrupt("return",(0,g.WY)("/api/skyflying/getCoarseCountsOfTableName",(0,d.Z)({method:"GET",params:D},E||{})));case 1:case"end":return T.stop()}},m)})),h.apply(this,arguments)}},6650:function(A,C,e){"use strict";e.d(C,{F$:function(){return Z},Xd:function(){return m},mt:function(){return E},et:function(){return W},FP:function(){return F},Bx:function(){return $}});var d=e(11849),u=e(3182),N=e(94043),i=e.n(N),g=e(25377);function Z(s,a){return h.apply(this,arguments)}function h(){return h=(0,u.Z)(i().mark(function s(a,l){return i().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,g.WY)("/api/skyflying/getCoarseCountsOfUsers",(0,d.Z)({method:"GET",params:a},l||{})));case 1:case"end":return t.stop()}},s)})),h.apply(this,arguments)}function m(s,a){return D.apply(this,arguments)}function D(){return D=(0,u.Z)(i().mark(function s(a,l){return i().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,g.WY)("/api/skyflying/getCountsOfUserRecentSevenDays",(0,d.Z)({method:"GET",params:a},l||{})));case 1:case"end":return t.stop()}},s)})),D.apply(this,arguments)}function E(s,a){return r.apply(this,arguments)}function r(){return r=(0,u.Z)(i().mark(function s(a,l){return i().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,g.WY)("/api/skyflying/getVisitRate",(0,d.Z)({method:"GET",params:{username:a}},l||{})));case 1:case"end":return t.stop()}},s)})),r.apply(this,arguments)}function T(s,a){return R.apply(this,arguments)}function R(){return R=_asyncToGenerator(_regeneratorRuntime.mark(function s(a,l){return _regeneratorRuntime.wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",request("/api/skyflying/getCountsOfAllRecentSevenDays",_objectSpread({method:"GET",params:a},l||{})));case 1:case"end":return t.stop()}},s)})),R.apply(this,arguments)}function W(s,a){return B.apply(this,arguments)}function B(){return B=(0,u.Z)(i().mark(function s(a,l){return i().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,g.WY)("/api/skyflying/getCountsOfUser",(0,d.Z)({method:"GET",params:a},l||{})));case 1:case"end":return t.stop()}},s)})),B.apply(this,arguments)}function F(s,a){return x.apply(this,arguments)}function x(){return x=(0,u.Z)(i().mark(function s(a,l){return i().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,g.WY)("/api/skyflying/getUserOperationTypeCount",(0,d.Z)({method:"GET",params:a},l||{})));case 1:case"end":return t.stop()}},s)})),x.apply(this,arguments)}function $(s,a){return S.apply(this,arguments)}function S(){return S=(0,u.Z)(i().mark(function s(a,l){return i().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,g.WY)("/api/skyflying/getUserUsualAndUnusualData",(0,d.Z)({method:"GET",params:a},l||{})));case 1:case"end":return t.stop()}},s)})),S.apply(this,arguments)}function ae(s,a){return O.apply(this,arguments)}function O(){return O=_asyncToGenerator(_regeneratorRuntime.mark(function s(a,l){return _regeneratorRuntime.wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",request("/api/skyflying/getCoarseCountsOfOneUser",_objectSpread({method:"GET",params:a||{}},l||{})));case 1:case"end":return t.stop()}},s)})),O.apply(this,arguments)}}}]);
