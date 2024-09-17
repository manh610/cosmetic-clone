package com.cosmetic.gg.service.address;
import java.util.List;

import com.cosmetic.gg.common.enums.EDeliveryType;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.entity.address.DeliveryUnit;

public interface DeliveryUnitService {

	List<DeliveryUnit> search(String keyword, EDeliveryType deliveryType);
	
	List<Error> validator(DeliveryUnit deliveryUnit);
	
	DeliveryUnit create(DeliveryUnit deliveryUnit);
	
	DeliveryUnit update(DeliveryUnit deliveryUnit);
	
	Error delete(String id);
}
