package com.cosmetic.gg.service.supply;

import java.util.List;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.entity.supply.Supplier;

public interface SupplierService {

	List<Supplier> search(String keyword, EStatus status);
	
	List<Error> validator(Supplier supplier);
	
	Supplier create(Supplier supplier);
	
	Supplier update(Supplier supplier);
	
	Error delete(String id);
	
	Supplier getById(String id);
}
