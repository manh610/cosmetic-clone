package com.cosmetic.gg.repository.address;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.address.SupplierAddress;

@Repository
public interface SupplierAddressRepository extends JpaRepository<SupplierAddress, String>{

}
