package com.cosmetic.gg.service.attribute;

import java.util.List;

import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.entity.attribute.ValueDetail;
import com.cosmetic.gg.model.attribute.ValueDetailModel;

public interface ValueDetailService {

	List<Error> validator(ValueDetailModel valueDetailModel);
	
	ValueDetail create(ValueDetailModel valueDetailModel);
	
	ValueDetail update(ValueDetailModel valueDetailModel);
	
	ValueDetailModel detail(String id);
}
