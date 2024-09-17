package com.cosmetic.gg.repository.attribute;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.attribute.SkinType;


@Repository
public interface SkinTypeRepository extends JpaRepository<SkinType, String>{

}
